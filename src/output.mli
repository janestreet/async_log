open! Core
open! Async_kernel
open! Import

type t [@@deriving sexp_of]

val create
  :  ?rotate:(unit -> unit Deferred.t)
  -> ?close:(unit -> unit Deferred.t)
  -> flush:(unit -> unit Deferred.t)
  -> (Message.t Queue.t -> unit Deferred.t)
  -> t

val create_expert
  :  ?rotate:(unit -> unit Deferred.t)
  -> ?close:(unit -> unit Deferred.t)
  -> flush:(unit -> unit Deferred.t)
  -> (Message_event.t Queue.t -> unit Deferred.t)
  -> t

val write : t -> Message_event.t Queue.t -> unit Deferred.t
val rotate : t -> unit Deferred.t
val flush : t -> unit Deferred.t
val filter_to_level : t -> level:Level.t -> t
val combine : t list -> t

module For_testing : sig
  val create : map_output:(string -> string) -> t
end
