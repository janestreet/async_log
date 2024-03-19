(** A library for general logging.

    Although this module is fully Async-safe it exposes almost no Deferreds.  This is
    partially a design choice to minimize the impact of logging in code, and partially the
    result of organic design (i.e., older versions of this interface did the same thing).

    A (limited) [Blocking] module is supplied to accommodate the portion of a program that
    runs outside of Async. *)

open! Core
open! Async_kernel
open! Import

type t [@@deriving sexp_of]

(** Sets the log level via a flag, if provided. *)
val set_level_via_param : t -> unit Command.Param.t

(** Messages sent at a level less than the current level will not be output. *)
val set_level : t -> Level.t -> unit

(** Returns the last level passed to [set_level], which will be the log level
    checked as a threshold against the level of the next message sent. *)
val level : t -> Level.t

(** Changes the output type of the log, which can be useful when daemonizing.
    The new output type will be applied to all subsequent messages. *)
val set_output : t -> Output.t list -> unit

val get_output : t -> Output.t list

(** Changes the time source of the log, which controls the default timestamp on
    messages. *)
val get_time_source : t -> Synchronous_time_source.t

val set_time_source : t -> Synchronous_time_source.t -> unit

(** Changes the [transform] function within log.  This allows you to *synchronously*
    change things about the message at the time that they were written.

    The transform function *will not* be called if the initial message is of a level that
    would not currently be logged.

    The transform function *will* be called if even if there are no log outputs. *)
val get_transform : t -> (Message_event.t -> Message_event.t) option

val set_transform : t -> (Message_event.t -> Message_event.t) option -> unit

(** If [`Raise] is given, then background errors raised by logging will be raised to the
    monitor that was in scope when [create] was called.  Errors can be redirected anywhere
    by providing [`Call f]. *)
val get_on_error : t -> [ `Raise | `Call of Error.t -> unit ]

val set_on_error : t -> [ `Raise | `Call of Error.t -> unit ] -> unit

(** Any call that writes to a log after [close] is called will raise. *)
val close : t -> unit Deferred.t

(** Returns true if [close] has been called. *)
val is_closed : t -> bool

(** Returns a [Deferred.t] that is fulfilled when the last message delivered to [t] before
    the call to [flushed] is out the door. *)
val flushed : t -> unit Deferred.t

(** Creates a new log.  See [set_level], [set_on_error], [set_output],
    [set_time_source], and [set_transform] for more. *)
val create
  :  level:Level.t
  -> output:Output.t list
  -> on_error:[ `Raise | `Call of Error.t -> unit ]
  -> ?time_source:Synchronous_time_source.t
  -> ?transform:(Message_event.t -> Message_event.t)
  -> unit
  -> t

(** Log that drops messages sent to it, as if it wrote to /dev/null *)
val create_null : unit -> t

(** Creates a copy of a log, which has the same settings and logs to the same outputs. *)
val copy : t -> t

(** Printf-like logging for messages at each log level or raw (no level) messages. Raw
    messages still include a timestamp. *)

val raw
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val debug
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val info
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

val error
  :  ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** Generalized printf-style logging. *)
val printf
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, unit) format4
  -> 'a

(** Sexp logging for messages at each log level or raw (no level) messages. Raw messages
    still include a timestamp *)

val raw_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val info_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val error_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit
val debug_s : ?time:Time_float.t -> ?tags:(string * string) list -> t -> Sexp.t -> unit

(** Generalized sexp-style logging. *)
val sexp
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> unit

(** Log a string directly. *)
val string
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> string
  -> unit

val structured_message
  :  ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> Message_data.t
  -> Message_source.t
  -> unit

(** Log a pre-created message. *)
val message : t -> Message.t -> unit

val message_event : t -> Message_event.t -> unit

(** [surround t message f] logs [message] and a UUID once before calling [f] and again
    after [f] returns or raises. If [f] raises, the second message will include the
    exception, and [surround] itself will re-raise the exception tagged with [message].
    [on_subsequent_errors] is passed to the internal monitor as [rest] argument. As usual,
    the logging happens only if [level] exceeds the minimum level of [t].*)
val surround_s
  :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
  -> ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> Sexp.t
  -> (unit -> 'a Deferred.t)
  -> 'a Deferred.t

val surroundf
  :  on_subsequent_errors:[ `Call of exn -> unit | `Log | `Raise ]
  -> ?level:Level.t
  -> ?time:Time_float.t
  -> ?tags:(string * string) list
  -> t
  -> ('a, unit, string, (unit -> 'b Deferred.t) -> 'b Deferred.t) format4
  -> 'a

(** [would_log] returns true if a message at the given log level would be logged if sent
    immediately.

    This will return [false] if there are no outputs for the log, unless there is
    a [transform] set. *)
val would_log : t -> Level.t option -> bool

module For_testing : sig
  (** [create_output ~map_output] creates a [Log.Output.t] which will print only
      [Message.message] to stdout, discarding any information about tags, levels, or
      timestamps.

      [map_output] will be applied to each string before printing, and is expected
      to be used to replace portions of output or identify which log a message came from.

      This function is best used with existing Log.ts, e.g. to replace Log.Global's
      outputs in expect tests. If you just want a full Log.t, see [create] below. *)
  val create_output : map_output:(string -> string) -> Output.t

  (** [create_log ~map_output level] creates a [Log.t] with its level set to [level] using
      the output returned by [create_output], and an [on_error] value of `Raise. *)
  val create : map_output:(string -> string) -> Level.t -> t
end

(**/**)

module Private : sig
  val push_message_event : t -> Message_event.t -> unit
  val set_async_trace_hook : (unit -> Univ.t option) -> unit
  val set_level_via_param_lazy : t Lazy.t -> unit Command.Param.t
  val all_live_logs_flushed : unit -> unit Deferred.t
end
