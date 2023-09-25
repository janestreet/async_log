(** The output module exposes a variant that describes the output type and sub-modules
    that each expose a write function (or create that returns a write function) that is
    of type: Level.t -> string -> unit Deferred.t.  It is the responsibility of the write
    function to contain all state, and to clean up after itself.
*)

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

val write : t -> Message.t Queue.t -> unit Deferred.t
val rotate : t -> unit Deferred.t
val flush : t -> unit Deferred.t
val filter_to_level : t -> level:Level.t -> t
val combine : t list -> t

module For_testing : sig
  val create : map_output:(string -> string) -> t
end
