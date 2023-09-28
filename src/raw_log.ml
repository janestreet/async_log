open! Core
open! Async_kernel
open! Async_unix
open! Import

(* A log is a pipe that can take one of four messages.
   | Msg (level, msg) -> write the message to the current output if the level is
   appropriate
   | New_output f -> set the output function for future messages to f
   | Flush i      -> used to get around the current odd design of Pipe flushing.  Sends an
   ivar that the reading side fills in after it has finished handling
   all previous messages.
   | Rotate  -> inform the output handlers to rotate exactly now

   The f delivered by New_output must not hold on to any resources that normal garbage
   collection won't clean up.  When New_output is delivered to the pipe the current
   write function will be discarded without notification.  If this proves to be a
   resource problem (too many syscalls for instance) then we could add an on_discard
   function to writers that we call when a new writer appears.
*)
module Update = struct
  type t =
    | Msg of Message.t
    | New_output of Output.t
    | Flush of unit Ivar.t
    | Rotate of unit Ivar.t
  [@@deriving sexp_of]

  let to_string t = Sexp.to_string (sexp_of_t t)
end

type t =
  { updates : Update.t Pipe.Writer.t
  ; mutable on_error : On_error.t
  ; mutable current_level : Level.t
  ; mutable output_is_disabled : bool
  ; mutable current_output : Output.t list
  ; mutable current_time_source : Synchronous_time_source.t
  ; mutable transform : (Message.t -> Message.t) option
  }

let equal t1 t2 = Pipe.equal t1.updates t2.updates
let hash t = Pipe.hash t.updates

let push_update t update =
  if not (Pipe.is_closed t.updates)
  then Pipe.write_without_pushback t.updates update
  else
    failwithf
      "Log: can't process %s because this log has been closed"
      (Update.to_string update)
      ()
;;

let flushed t = Deferred.create (fun i -> push_update t (Flush i))
let rotate t = Deferred.create (fun rotated -> push_update t (Rotate rotated))
let is_closed t = Pipe.is_closed t.updates

module Flush_at_exit_or_gc : sig
  val add_log : t -> unit
  val close : t -> unit Deferred.t
end = struct
  module Weak_table = Stdlib.Weak.Make (struct
    type z = t
    type t = z

    let equal = equal
    let hash = hash
  end)

  (* contains all logs we want to flush at shutdown *)
  let flush_bag = lazy (Bag.create ())

  (* contains all currently live logs. *)
  let live_logs = lazy (Weak_table.create 1)

  (* [flush] adds a flush deferred to the flush_bag *)
  let flush t =
    if not (is_closed t)
    then (
      let flush_bag = Lazy.force flush_bag in
      let flushed = flushed t in
      let tag = Bag.add flush_bag flushed in
      upon flushed (fun () -> Bag.remove flush_bag tag);
      flushed)
    else return ()
  ;;

  let close t =
    if not (is_closed t)
    then (
      (* this will cause the log to flush its outputs, but because they may have been
         reused it does not close them, they'll be closed automatically when they fall out
         of scope. *)
      Pipe.write_without_pushback t.updates (New_output (Output.combine []));
      let finished = flushed t in
      Pipe.close t.updates;
      finished)
    else return ()
  ;;

  let finish_at_shutdown =
    lazy
      (Shutdown.at_shutdown (fun () ->
         let live_logs = Lazy.force live_logs in
         let flush_bag = Lazy.force flush_bag in
         Weak_table.iter (fun log -> don't_wait_for (flush log)) live_logs;
         Deferred.all_unit (Bag.to_list flush_bag)))
  ;;

  let add_log log =
    let live_logs = Lazy.force live_logs in
    Lazy.force finish_at_shutdown;
    Weak_table.remove live_logs log;
    Weak_table.add live_logs log;
    (* If we fall out of scope just close and flush normally.  Without this we risk being
       finalized and removed from the weak table before the the shutdown handler runs, but
       also before we get all of logs out of the door. *)
    Gc.add_finalizer_exn log (fun log -> don't_wait_for (close log))
  ;;
end

let close = Flush_at_exit_or_gc.close

let create_log_processor ~output =
  let batch_size = 100 in
  let output = ref (Output.combine output) in
  let msgs = Queue.create () in
  (* Some special scheduling properties of this processor loop that may explain why it's
     not a [Deferred.repeat_until_finished] is
     (1) if a [Msg] is processed, there's no new async job created
     (2) If [Queue.length msgs = 0], then [f] is called immediately. *)
  let output_message_queue f =
    if Queue.length msgs = 0
    then f ()
    else (
      let%bind () = Output.write !output msgs in
      Queue.clear msgs;
      f ())
  in
  stage (fun (updates : Update.t Queue.t) ->
    let rec loop yield_every =
      let yield_every = yield_every - 1 in
      if yield_every = 0
      then (
        (* this introduces a yield point so that other async jobs have a chance to run
           under circumstances when large batches of logs are delivered in bursts. *)
        let%bind () = Async_kernel_scheduler.yield () in
        loop batch_size)
      else (
        match Queue.dequeue updates with
        | None -> output_message_queue (fun _ -> return ())
        | Some update ->
          (match update with
           | Rotate i ->
             output_message_queue (fun () ->
               let%bind () = Output.rotate !output in
               Ivar.fill_exn i ();
               loop yield_every)
           | Flush i ->
             output_message_queue (fun () ->
               let%bind () = Output.flush !output in
               Ivar.fill_exn i ();
               loop yield_every)
           | Msg msg ->
             Queue.enqueue msgs msg;
             loop yield_every
           | New_output o ->
             output_message_queue (fun () ->
               (* we don't close the output because we may re-use it.  We rely on the
                  finalizer on the output to call close once it falls out of scope. *)
               let%bind () = Output.flush !output in
               output := o;
               loop yield_every)))
    in
    loop batch_size)
;;

let process_log_redirecting_all_errors t r output =
  Monitor.try_with ~rest:`Log (fun () ->
    let process_log = create_log_processor ~output |> unstage in
    Pipe.iter' r ~f:process_log)
  >>| Result.iter_error ~f:(On_error.handle_error t.on_error)
;;

let create ~level ~output ~on_error ~time_source ~transform : t =
  (* this has no optional args so that we make sure to update/consider all internal call
     sites if the signature changes *)
  let r, w = Pipe.create () in
  let time_source =
    match time_source with
    | Some time_source -> time_source
    | None -> Synchronous_time_source.default
  in
  let t =
    { updates = w
    ; on_error
    ; current_level = level
    ; output_is_disabled = List.is_empty output
    ; current_output = output
    ; current_time_source = time_source
    ; transform
    }
  in
  Flush_at_exit_or_gc.add_log t;
  don't_wait_for (process_log_redirecting_all_errors t r output);
  t
;;

let set_output t outputs =
  t.output_is_disabled <- List.is_empty outputs;
  t.current_output <- outputs;
  push_update t (New_output (Output.combine outputs))
;;

let get_output t = t.current_output
let get_on_error t = t.on_error
let set_on_error t handler = t.on_error <- handler
let level t = t.current_level
let set_level t level = t.current_level <- level
let get_time_source t = t.current_time_source
let set_time_source t time_source = t.current_time_source <- time_source
let get_transform t = t.transform
let set_transform t f = t.transform <- f

let copy t =
  create
    ~level:(level t)
    ~output:(get_output t)
    ~on_error:(get_on_error t)
    ~time_source:(Some (get_time_source t))
    ~transform:(get_transform t)
;;

(* would_log is broken out and tested separately for every sending function to avoid the
   overhead of message allocation when we are just going to drop the message. *)
let would_log t msg_level =
  let output_or_transform_is_enabled =
    (not t.output_is_disabled) || Option.is_some t.transform
  in
  output_or_transform_is_enabled
  && Level.as_or_more_verbose_than ~log_level:(level t) ~msg_level
;;

let push_message t msg =
  (* We want to call [transform], even if we don't end up pushing the message to an
     output.  This allows for someone to listen to all messages that would theoretically
     be logged by this log (respecting level), and then maybe log them somewhere else. *)
  let msg =
    match t.transform with
    | None -> msg
    | Some f -> f msg
  in
  if not t.output_is_disabled then push_update t (Msg msg)
;;
