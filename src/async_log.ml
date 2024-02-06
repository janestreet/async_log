include Log
module Blocking = Blocking
module Global = Global
module Level = Async_log_kernel.Level
module Message = Message
module Message_event = Async_log_kernel.Message_event
module Output = Output

module Ppx_log_syntax = struct
  module Ppx_log_syntax = Ppx_log_syntax
end

module Reader = Message_reader
module Rotation = Rotation
module Rotation_id = Rotation_id
include Assign_top_level_logs
