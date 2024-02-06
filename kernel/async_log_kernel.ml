module Global = Global
module Level = Level
module Log = Log
module Message = Message
module Message_event = Message_event

module Output = struct
  include Output
  module Format = Output_format
end

module For_testing = struct
  module Mutable_outputs = Mutable_outputs
end
