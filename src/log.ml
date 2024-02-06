open! Core
open! Async_kernel
open! Import
include Async_log_kernel.Log

let () = Shutdown.at_shutdown Private.all_live_logs_flushed
