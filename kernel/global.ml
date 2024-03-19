open! Core
open! Async_kernel
open! Import

module type S = Global_intf.S

module Make () = struct
  let log =
    lazy
      (Log.create
         ?time_source:None
         ?transform:None
         ()
         ~level:`Info
         ~output:[ force Output.stderr ]
         ~on_error:On_error.send_to_top_level_monitor)
  ;;

  let level () = Log.level (Lazy.force log)
  let set_level level = Log.set_level (Lazy.force log) level
  let set_output output = Log.set_output (Lazy.force log) output
  let get_output () = Log.get_output (Lazy.force log)
  let set_on_error handler = Log.set_on_error (Lazy.force log) handler
  let get_time_source () = Log.get_time_source (Lazy.force log)
  let set_time_source time_source = Log.set_time_source (Lazy.force log) time_source
  let get_transform () = Log.get_transform (Lazy.force log)
  let set_transform transform = Log.set_transform (Lazy.force log) transform
  let would_log level = Log.would_log (Lazy.force log) level
  let raw ?time ?tags k = Log.raw ?time ?tags (Lazy.force log) k
  let info ?time ?tags k = Log.info ?time ?tags (Lazy.force log) k
  let error ?time ?tags k = Log.error ?time ?tags (Lazy.force log) k
  let debug ?time ?tags k = Log.debug ?time ?tags (Lazy.force log) k
  let raw_s ?time ?tags the_sexp = Log.sexp ?time ?tags (Lazy.force log) the_sexp

  let debug_s ?time ?tags the_sexp =
    Log.sexp ~level:`Debug ?time ?tags (Lazy.force log) the_sexp
  ;;

  let info_s ?time ?tags the_sexp =
    Log.sexp ~level:`Info ?time ?tags (Lazy.force log) the_sexp
  ;;

  let error_s ?time ?tags the_sexp =
    Log.sexp ~level:`Error ?time ?tags (Lazy.force log) the_sexp
  ;;

  let flushed () = Log.flushed (Lazy.force log)
  let printf ?level ?time ?tags k = Log.printf ?level ?time ?tags (Lazy.force log) k
  let sexp ?level ?time ?tags s = Log.sexp ?level ?time ?tags (Lazy.force log) s
  let string ?level ?time ?tags s = Log.string ?level ?time ?tags (Lazy.force log) s
  let message msg = Log.message (Lazy.force log) msg
  let message_event msg = Log.message_event (Lazy.force log) msg

  let structured_message ?level ?time ?tags =
    Log.structured_message ?level ?time ?tags (Lazy.force log)
  ;;

  let surround_s ~on_subsequent_errors ?level ?time ?tags msg f =
    Log.surround_s ~on_subsequent_errors ?level ?time ?tags (Lazy.force log) msg f
  ;;

  let surroundf ~on_subsequent_errors ?level ?time ?tags fmt =
    Log.surroundf ~on_subsequent_errors ?level ?time ?tags (Lazy.force log) fmt
  ;;

  let set_level_via_param () = Log.Private.set_level_via_param_lazy log

  module For_testing = struct
    let use_test_output ?(map_output = Fn.id) () =
      set_output [ Output.For_testing.create ~map_output ]
    ;;
  end
end

include Make ()
