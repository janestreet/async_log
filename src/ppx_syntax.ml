open! Base
open! Import

module T = struct
  type t = Log.t
  type time = Time.t
  type return_type = unit

  let would_log = Log.would_log
  let printf = Log.printf
  let sexp = Log.sexp
  let default = ()
end

include T

module Global = struct
  type return_type = unit

  let default = ()
  let would_log = Global.would_log
  let printf = Global.printf
  let sexp = Global.sexp
end

module No_global = struct
  module Ppx_log_syntax = struct
    include T

    module Global = struct
      type return_type = [ `Do_not_use_because_it_will_not_log ]

      let default = `Do_not_use_because_it_will_not_log
      let would_log _ = false
      let sexp ?level:_ ?time:_ ?tags:_ _ = `Do_not_use_because_it_will_not_log

      let printf ?level:_ ?time:_ ?tags:_ =
        Core.ksprintf (Fn.const `Do_not_use_because_it_will_not_log)
      ;;
    end
  end
end
