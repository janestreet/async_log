open! Core
open! Async_kernel
open! Import
include Async_log_kernel.Global
module Make () = (val make (lazy (Output.stderr ())))
include Make ()
