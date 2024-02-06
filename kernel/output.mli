open! Core
open! Async_kernel
open! Import

type t [@@deriving sexp_of]

val create
  :  ?rotate:(unit -> unit Deferred.t)
  -> ?finalize:(unit -> unit Deferred.t)
  -> flush:(unit -> unit Deferred.t)
  -> (Message.t Queue.t -> unit Deferred.t)
  -> t

val create_expert
  :  ?rotate:(unit -> unit Deferred.t)
  -> ?finalize:(unit -> unit Deferred.t)
  -> flush:(unit -> unit Deferred.t)
  -> (Message_event.t Queue.t -> unit Deferred.t)
  -> t

val create_unbuffered
  :  ?finalize:(unit -> unit Deferred.t)
  -> flush:(unit -> unit Deferred.t)
  -> (Message_event.t -> unit)
  -> t

val empty : t
val write : t -> Message_event.t -> unit
val rotate : t -> unit Deferred.t
val flush : t -> unit Deferred.t
val filter_to_level : t -> level:Level.t -> t

module Private : sig
  (** If a background error occurs, [background_error] becomes determined, log processing
      stops and further calls to [flush] will hang. *)
  val buffered_background_error
    :  t
    -> [ `Output_is_unbuffered | `Error of exn Deferred.t ]
end

module For_testing : sig
  val create : map_output:(string -> string) -> t
end
