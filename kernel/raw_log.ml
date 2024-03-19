open! Core
open! Async_kernel
open! Import
module Id = Unique_id.Int ()

type t =
  { id : Id.t
  ; on_error : On_error.t ref
  ; mutable is_closed : bool
  ; mutable level : Level.t
  ; output : Mutable_outputs.t
  ; mutable time_source : Synchronous_time_source.t
  ; mutable transform : (Message_event.t -> Message_event.t) option
  }

let assert_open t tag =
  if t.is_closed then failwithf "Log: can't %s because this log has been closed" tag ()
;;

let flushed t =
  assert_open t "flush";
  Mutable_outputs.flushed t.output
;;

let is_closed t = t.is_closed

let flush_and_close t =
  if not (is_closed t)
  then (
    let finished = flushed t in
    t.is_closed <- true;
    finished)
  else return ()
;;

let close = flush_and_close

let live_logs =
  lazy
    (Live_entry_registry.create
       (module struct
         type nonrec t = t

         let equal t1 t2 = Id.equal t1.id t2.id
         let hash t1 = Id.hash t1.id
         let flushed = flushed
         let is_closed = is_closed
         let flush_and_close = flush_and_close
       end))
;;

let create ~level ~output ~on_error ~time_source ~transform =
  let time_source =
    match time_source with
    | Some time_source -> time_source
    | None ->
      if Ppx_inline_test_lib.am_running
      then Synchronous_time_source.(read_only (create ~now:Time_ns.epoch ()))
      else Synchronous_time_source.wall_clock ()
  in
  let on_error = ref on_error in
  let output =
    Mutable_outputs.create output ~on_background_output_error:(fun exn ->
      On_error.handle_error !on_error exn)
  in
  let id = Id.create () in
  let t = { id; on_error; level; output; time_source; transform; is_closed = false } in
  Live_entry_registry.register (force live_logs) t;
  t
;;

let set_output t new_outputs =
  assert_open t "set output";
  Mutable_outputs.update_outputs t.output new_outputs
;;

let get_output t = Mutable_outputs.current_outputs t.output
let get_on_error t = !(t.on_error)
let set_on_error t handler = t.on_error := handler
let level t = t.level
let set_level t level = t.level <- level
let get_time_source t = t.time_source
let set_time_source t time_source = t.time_source <- time_source
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
    (not (Mutable_outputs.is_empty t.output)) || Option.is_some t.transform
  in
  output_or_transform_is_enabled
  && Level.as_or_more_verbose_than ~log_level:(level t) ~msg_level
;;

let push_message_event t msg =
  (* We want to call [transform], even if we don't end up pushing the message to an
     output.  This allows for someone to listen to all messages that would theoretically
     be logged by this log (respecting level), and then maybe log them somewhere else. *)
  let msg =
    match t.transform with
    | None -> msg
    | Some f -> f msg
  in
  if not (Mutable_outputs.is_empty t.output)
  then (
    assert_open t "write message";
    Mutable_outputs.write t.output msg)
;;

let all_live_logs_flushed () =
  match Lazy.peek live_logs with
  | Some live_logs -> Live_entry_registry.live_entries_flushed live_logs
  | None -> Deferred.unit
;;
