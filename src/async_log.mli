open! Core
open! Async_kernel
open! Import
include module type of Log
module Blocking = Blocking
module Global = Global
module Level = Async_log_kernel.Level
module Message = Message
module Message_event = Async_log_kernel.Message_event
module Output = Output
module Reader = Message_reader
module Rotation = Rotation
module Rotation_id = Rotation_id

module Ppx_log_syntax : sig
  (** [Async_log.Ppx_log_syntax.Ppx_log_syntax] exists so that people can [open
      Async_log.Ppx_log_syntax] to use ppx log, instead of doing a module alias. This is
      consistent with [Monad.Syntax.Let_syntax]. *)
  module Ppx_log_syntax : module type of Ppx_log_syntax
end
