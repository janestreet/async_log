open! Core
open Async_kernel
open! Import

type t

val create : Output.t list -> on_background_output_error:(exn -> unit) -> t
val is_empty : t -> bool
val write : t -> Message_event.t -> unit
val flushed : t -> unit Deferred.t
val current_outputs : t -> Output.t list
val update_outputs : t -> Output.t list -> unit
