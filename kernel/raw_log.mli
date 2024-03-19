open! Core
open Async_kernel
open! Import

type t

val create
  :  level:Level.t
  -> output:Output.t list
  -> on_error:On_error.t
  -> time_source:Synchronous_time_source.t option
  -> transform:(Message_event.t -> Message_event.t) option
  -> t

val copy : t -> t
val level : t -> Level.t
val set_level : t -> Level.t -> unit
val set_output : t -> Output.t list -> unit
val get_output : t -> Output.t list
val get_time_source : t -> Synchronous_time_source.t
val set_time_source : t -> Synchronous_time_source.t -> unit
val get_transform : t -> (Message_event.t -> Message_event.t) option
val set_transform : t -> (Message_event.t -> Message_event.t) option -> unit
val get_on_error : t -> On_error.t
val set_on_error : t -> On_error.t -> unit
val close : t -> unit Deferred.t
val is_closed : t -> bool
val flushed : t -> unit Deferred.t
val would_log : t -> Level.t option -> bool
val push_message_event : t -> Message_event.t -> unit
val all_live_logs_flushed : unit -> unit Deferred.t
