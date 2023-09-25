open! Core
open! Async_kernel
open! Import

(** An interface for singleton logs. *)
module type S = sig
  val log : Log.t Lazy.t
  val level : unit -> Level.t
  val set_level : Level.t -> unit
  val set_output : Output.t list -> unit
  val get_output : unit -> Output.t list
  val set_on_error : [ `Raise | `Call of Error.t -> unit ] -> unit
  val get_time_source : unit -> Synchronous_time_source.t
  val set_time_source : Synchronous_time_source.t -> unit
  val get_transform : unit -> (Message.t -> Message.t) option
  val set_transform : (Message.t -> Message.t) option -> unit
  val would_log : Level.t option -> bool
  val set_level_via_param : unit -> unit Command.Param.t

  (** Functions that operate on a given log.  In this case they operate on a single log
      global to the module. *)

  val raw
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val info
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val error
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val debug
    :  ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val flushed : unit -> unit Deferred.t
  val rotate : unit -> unit Deferred.t

  val printf
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, unit) format4
    -> 'a

  val raw_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val info_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val error_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit
  val debug_s : ?time:Time.t -> ?tags:(string * string) list -> Sexp.t -> unit

  val sexp
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> unit

  val string
    :  ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> string
    -> unit

  val message : Message.t -> unit

  val surround_s
    :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
    -> ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> Sexp.t
    -> (unit -> 'a Deferred.t)
    -> 'a Deferred.t

  val surroundf
    :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
    -> ?level:Level.t
    -> ?time:Time.t
    -> ?tags:(string * string) list
    -> ('a, unit, string, (unit -> 'b Deferred.t) -> 'b Deferred.t) format4
    -> 'a

  module For_testing : sig
    (** Change the output of the global log so that it only prints the bodies of messages
        to stdout, discarding any information about tags, levels, or timestamps.
        [map_output] can be used to transform messages before they make it to stdout; by
        default it is [Fn.id].

        This is equivalent to:
        [Log.Global.set_output [ Log.For_testing.create_output ~map_output ]] *)
    val use_test_output : ?map_output:(string -> string) -> unit -> unit
  end
end

module type Global = sig
  module type S = S

  (** This functor can be called to generate "singleton" logging modules. *)
  module Make () : S

  (** Programs that want simplistic single-channel logging can open this module.  It
      provides a global logging facility to a single output type at a single level.  More
      nuanced logging can be had by using the functions that operate on a distinct [Log.t]
      type. *)
  include S
end
