open! Core
open! Import

(** A [t] represents a log message with additional structured information about the
    statement, for use by outputs. Most outputs flatten the event to a [Message.t], which
    predates this module and is serializable / deserializable, but has less structured
    information.

    (This module's signature is tight to keep the type extensible, but this isn't intended
    to deter any users who may wish for an accessor to be added.)
*)
type t

val create
  :  ?time:Time.t
  -> ?source:string
  -> ?legacy_tags:(string * string) list
  -> ?level:Level.t
  -> [ Sexp_or_string.t | `Structured of Message_sexp.t ]
  -> t

val raw_message : t -> [ Sexp_or_string.t | `Structured of Message_sexp.t ]
val source : t -> Message_source.t
val legacy_tags : t -> (string * string) list
val level : t -> Level.t option
val time : t -> Time.t
val to_serialized_message_lossy : t -> Message.t
val of_serialized_message : Message.t -> t

module Unstable : sig
  (** [sexp_of] is here since we explicitly do not want this serialized in a way where
      users may expect to be able to deserialize. *)
  type nonrec t = t [@@deriving sexp_of]
end

module Private : sig
  val create
    :  [ Sexp_or_string.t | `Structured of Message_sexp.t ]
    -> Message_source.t
    -> time:Time_float.t
    -> level:Level.t option
    -> legacy_tags:(string * string) list
    -> is_from_upstream_log:bool
    -> t

  val is_from_upstream_log : t -> bool
end
