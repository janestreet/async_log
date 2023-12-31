module Blocking = Blocking
module Global = Global

module type Global_intf = Global.S

module Level = Level
module Make_global = Global.Make

module Message = struct
  include Message
  module Private = Message
end

module Message_event = Message_event
module Message_sexp = Ppx_log_types.Message_sexp

module Output = struct
  include Output
  include Output_unix
  module Format = Output_format
end

module Ppx_log_syntax = struct
  module Ppx_log_syntax = Ppx_log_syntax
end

module Reader = Message_reader

module Rotation = struct
  module type Id_intf = Rotation_id.S

  include Rotation
end

include Log

(* see comment in [assign_top_level_logs.mli] *)
include Assign_top_level_logs
