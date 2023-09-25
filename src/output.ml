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
  { write : Message.t Queue.t -> unit Deferred.t
  ; rotate : unit -> unit Deferred.t
  ; close : unit -> unit Deferred.t
  ; flush : unit -> unit Deferred.t
  ; (* experimentation shows that this record, without this field, can sometimes raise
       when passed to Heap_block.create_exn, which we need to do to add a finalizer.
       This seems to occur when the functions are top-level and/or constant.  More
       investigation is probably worthwhile. *)
    heap_block : Definitely_a_heap_block.t
  }

let create ?(rotate = fun () -> return ()) ?(close = fun () -> return ()) ~flush write =
  let t =
    { write; rotate; close; flush; heap_block = Definitely_a_heap_block.the_one_and_only }
  in
  Gc.add_finalizer (Heap_block.create_exn t) (fun t ->
    let t = Heap_block.value t in
    don't_wait_for
      (let%bind () = t.flush () in
       t.close ()));
  t
;;

let write t msgs = t.write msgs
let rotate t = t.rotate ()
let flush t = t.flush ()
let sexp_of_t _ = Sexp.Atom "<opaque>"

let combine ts =
  (* There is a crazy test that verifies that we combine things correctly when the same
     rotate output is included 5 times in Log.create, so we must make this Sequential to
     enforce the rotate invariants and behavior. *)
  let iter_combine_exns =
    (* No need for the Monitor overhead in the case of a single t *)
    match ts with
    | [] -> fun (_ : t -> unit Deferred.t) -> Deferred.unit
    | [ single_t ] -> fun f -> f single_t
    | ts ->
      fun f ->
        Deferred.List.map ~how:`Sequential ts ~f:(fun t ->
          Monitor.try_with_or_error ~rest:`Log (fun () -> f t))
        >>| Or_error.combine_errors_unit
        >>| Or_error.ok_exn
  in
  let write msg = iter_combine_exns (fun t -> t.write msg) in
  let rotate () = iter_combine_exns (fun t -> t.rotate ()) in
  let close () = iter_combine_exns (fun t -> t.close ()) in
  let flush () = iter_combine_exns (fun t -> t.flush ()) in
  { write; rotate; close; flush; heap_block = Definitely_a_heap_block.the_one_and_only }
;;

let filter_to_level t ~level =
  let write messages =
    let filtered_messages =
      Queue.filter messages ~f:(fun message ->
        Level.as_or_more_verbose_than ~log_level:level ~msg_level:(Message.level message))
    in
    t.write filtered_messages
  in
  create ~rotate:t.rotate ~close:t.close ~flush:t.flush write
;;

module For_testing = struct
  let create ~map_output =
    let each_print_endline_automatically_flushes () = Deferred.unit in
    create ~flush:each_print_endline_automatically_flushes (fun queue ->
      Queue.iter queue ~f:(fun message ->
        map_output (Message.message message) |> print_endline);
      Deferred.unit)
  ;;
end
