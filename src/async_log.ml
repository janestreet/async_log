module Blocking = Blocking
module Global = Global

module type Global_intf = Global.S

module Level = Level
module Make_global = Global.Make

module Message = struct
  include Message
  module Private = Message
end

module Output = struct
  include Output
  include Output_unix
  module Format = Output_format
end

module Reader = Message_reader

module Rotation = struct
  module type Id_intf = Rotation_id.S

  include Rotation
end

include Log

(* see comment in [assign_top_level_logs.mli] *)
include Assign_top_level_logs
