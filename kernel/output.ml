open! Core
open! Async_kernel
open! Import

module Definitely_a_heap_block : sig
  type t

  val the_one_and_only : t
end = struct
  type t = string

  let the_one_and_only = String.make 1 ' '
end

type t =
  { write : Message_event.t -> unit
  ; rotate : unit -> unit Deferred.t
  ; flush : unit -> unit Deferred.t
  ; buffered_background_error : [ `Output_is_unbuffered | `Error of exn Deferred.t ]
      (* experimentation shows that this record, without this field, can sometimes raise
     when passed to Heap_block.create_exn, which we need to do to add a finalizer.
     This seems to occur when the functions are top-level and/or constant.  More
     investigation is probably worthwhile. *)
  ; heap_block : Definitely_a_heap_block.t
  }

let aux_create ~finalize ~rotate ~flush ~buffered_background_error ~write =
  let heap_block = Definitely_a_heap_block.the_one_and_only in
  let t = { write; rotate; flush; buffered_background_error; heap_block } in
  Option.iter finalize ~f:(fun finalize ->
    Gc.add_finalizer (Heap_block.create_exn t) (fun t ->
      let t = Heap_block.value t in
      don't_wait_for
        (let%bind () = t.flush () in
         finalize ())));
  t
;;

let create_expert ?(rotate = return) ?finalize ~flush write =
  let output = Buffered_output.create ~rotate ~flush ~write in
  aux_create
    ~finalize
    ~rotate:(fun () -> Buffered_output.rotate output)
    ~flush:(fun () -> Buffered_output.flushed output)
    ~buffered_background_error:(`Error (Buffered_output.background_error output))
    ~write:(Buffered_output.write output)
;;

let create_unbuffered ?finalize ~flush write =
  aux_create
    ~finalize
    ~rotate:return
    ~flush
    ~buffered_background_error:`Output_is_unbuffered
    ~write
;;

let create ?rotate ?finalize ~flush write =
  create_expert ?rotate ?finalize ~flush (fun messages ->
    Queue.map messages ~f:Message_event.to_serialized_message_lossy |> write)
;;

let empty =
  create_expert (fun (_ : Message_event.t Queue.t) -> Deferred.unit) ~flush:return
;;

let write t = t.write
let rotate t = t.rotate ()
let flush t = t.flush ()
let sexp_of_t _ = Sexp.Atom "<opaque>"

let filter_to_level t ~level =
  let write message =
    if Level.as_or_more_verbose_than
         ~log_level:level
         ~msg_level:(Message_event.level message)
    then t.write message
  in
  { t with write }
;;

module Private = struct
  let buffered_background_error t = t.buffered_background_error
end

module For_testing = struct
  let create ~map_output =
    let each_print_endline_automatically_flushes () = Deferred.unit in
    create ~flush:each_print_endline_automatically_flushes (fun queue ->
      Queue.iter queue ~f:(fun message ->
        map_output (Message.message message) |> print_endline);
      Deferred.unit)
  ;;
end
