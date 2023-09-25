open! Core
open! Async_kernel
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
  ; mutable on_error : [ `Raise | `Call of Error.t -> unit ]
  ; mutable current_level : Level.t
  ; mutable output_is_disabled : bool
  ; mutable current_output : Output.t list
  ; mutable current_time_source : Synchronous_time_source.t
  ; mutable transform : (Message.t -> Message.t) option
  }

let equal t1 t2 = Pipe.equal t1.updates t2.updates
let hash t = Pipe.hash t.updates
let sexp_of_t _t = Sexp.Atom "<opaque>"

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
  let output_message_queue f =
    if Queue.length msgs = 0
    then f ()
    else (
      let%bind () = Output.write !output msgs in
      Queue.clear msgs;
      f ())
  in
  fun (updates : Update.t Queue.t) ->
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
    loop batch_size
;;

let process_log_redirecting_all_errors t r output =
  match%map
    Monitor.try_with ~rest:`Log (fun () ->
      let process_log = create_log_processor ~output in
      Pipe.iter' r ~f:process_log)
  with
  | Ok () -> ()
  | Error e ->
    (match t.on_error with
     | `Raise -> raise e
     | `Call f -> f (Error.of_exn e))
;;

let create_internal ~level ~output ~on_error ~time_source ~transform : t =
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

module For_external_use_only = struct
  (* a more convenient interface for use externally *)
  let create ~level ~output ~on_error ?time_source ?transform () : t =
    create_internal ~level ~output ~on_error ~time_source ~transform
  ;;
end

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
  create_internal
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

let message t msg = if would_log t (Message.level msg) then push_message t msg

let create_message t ?level ?time ?tags msg =
  let time_source = get_time_source t in
  Message.create ?level ?time ~time_source ?tags msg
;;

let push_sexp ?level ?time ?tags t sexp =
  push_message t (create_message t ?level ?time ?tags (`Sexp sexp))
;;

let sexp ?level ?time ?tags t sexp =
  if would_log t level then push_sexp ?level ?time ?tags t sexp
;;

let string ?level ?time ?tags t s =
  if would_log t level
  then push_message t (create_message t ?level ?time ?tags (`String s))
;;

let printf ?level ?time ?tags t fmt =
  if would_log t level
  then
    ksprintf
      (fun msg -> push_message t (create_message t ?level ?time ?tags (`String msg)))
      fmt
  else ifprintf () fmt
;;

let add_uuid_to_tags tags =
  let uuid =
    match Base.Exported_for_specific_uses.am_testing with
    | true -> Uuid.Stable.V1.for_testing
    | false -> Uuid_unix.create ()
  in
  ("Log.surround_id", Uuid.to_string uuid) :: tags
;;

let surround_s_gen
  ?(tags = [])
  ~try_with
  ~map_return
  ~(log_sexp : ?tags:(string * string) list -> Sexp.t -> unit)
  ~f
  msg
  =
  let tags = add_uuid_to_tags tags in
  log_sexp ~tags [%message "Enter" ~_:(msg : Sexp.t)];
  map_return (try_with f) ~f:(function
    | Ok x ->
      log_sexp ~tags [%message "Exit" ~_:(msg : Sexp.t)];
      x
    | Error exn ->
      log_sexp ~tags [%message "Raised while " ~_:(msg : Sexp.t) (exn : exn)];
      Exn.reraise exn (sprintf !"%{sexp:Sexp.t}" msg))
;;

let surroundf_gen
  ?(tags = [])
  ~try_with
  ~map_return
  ~(log_string : ?tags:(string * string) list -> string -> unit)
  =
  ksprintf (fun msg f ->
    let tags = add_uuid_to_tags tags in
    log_string ~tags ("Enter " ^ msg);
    map_return (try_with f) ~f:(function
      | Ok x ->
        log_string ~tags ("Exit " ^ msg);
        x
      | Error exn ->
        log_string ~tags ("Raised while " ^ msg ^ ":" ^ Exn.to_string exn);
        Exn.reraise exn msg))
;;

let surround_s ~on_subsequent_errors ?level ?time ?tags t msg f =
  surround_s_gen
    ?tags
    ~try_with:(Monitor.try_with ~run:`Schedule ~rest:on_subsequent_errors)
    ~map_return:Deferred.map
    ~log_sexp:(fun ?tags s -> sexp ?tags ?level ?time t s)
    ~f
    msg
;;

let surroundf ~on_subsequent_errors ?level ?time ?tags t fmt =
  surroundf_gen
    ?tags
    ~try_with:(Monitor.try_with ~run:`Schedule ~rest:on_subsequent_errors)
    ~map_return:Deferred.map
    ~log_string:(fun ?tags -> string ?tags ?level ?time t)
    fmt
;;

let set_level_via_param_helper ~f =
  let open Command.Param in
  map
    (flag "log-level" (optional Level.arg) ~doc:"LEVEL The log level")
    ~f:(Option.iter ~f)
;;

let set_level_via_param log = set_level_via_param_helper ~f:(set_level log)

let set_level_via_param_lazy log =
  set_level_via_param_helper ~f:(fun level -> set_level (Lazy.force log) level)
;;

let raw ?time ?tags t fmt = printf ?time ?tags t fmt
let debug ?time ?tags t fmt = printf ~level:`Debug ?time ?tags t fmt
let info ?time ?tags t fmt = printf ~level:`Info ?time ?tags t fmt
let error ?time ?tags t fmt = printf ~level:`Error ?time ?tags t fmt
let raw_s ?time ?tags t the_sexp = sexp ?time ?tags t the_sexp
let debug_s ?time ?tags t the_sexp = sexp ~level:`Debug ?time ?tags t the_sexp
let info_s ?time ?tags t the_sexp = sexp ~level:`Info ?time ?tags t the_sexp
let error_s ?time ?tags t the_sexp = sexp ~level:`Error ?time ?tags t the_sexp

let%bench_module "unused log messages" =
  (module struct
    let (log : t) =
      create_internal
        ~level:`Info
        ~output:[ Output_unix.file `Text ~filename:"/dev/null" ]
        ~on_error:`Raise
        ~time_source:None
        ~transform:None
    ;;

    let%bench "unused printf" = debug log "blah"
    let%bench "unused printf w/subst" = debug log "%s" "blah"
    let%bench "unused string" = string log ~level:`Debug "blah"
    let%bench "used printf" = info log "blah"
  end)
;;

module For_testing = struct
  let create_output = Output.For_testing.create

  let create ~map_output level =
    let output = [ create_output ~map_output ] in
    create_internal ~output ~level ~on_error:`Raise ~time_source:None ~transform:None
  ;;
end

module Private = struct
  let push_upstream_log_sexp t sexp ~level ~time ~tags =
    push_message t (Message.create_from_upstream_log (`Sexp sexp) ~level ~time ~tags)
  ;;

  let set_level_via_param_lazy = set_level_via_param_lazy
end

include For_external_use_only

let create_null () =
  create ~level:`Error ~output:[] ~on_error:(`Call (fun (_ : Error.t) -> ())) ()
;;
