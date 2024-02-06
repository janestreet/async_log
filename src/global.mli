open! Core
open! Async_kernel
open! Import
include module type of Async_log_kernel.Global

(** [Async_log.Global.Make] is a shorthand for [Async_log_kernel.Global.make] with a
    default output of stderr. Similar to below, we recommend using [ppx_log] instead with
    an explicit [Log.t]. *)
module Make () : S

(** This module provides functions like [Global.sexp] which logs without needing to
    provide a [Log.t]. At this point, it's recommended to use [ppx_log] instead. *)
include S
