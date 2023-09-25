module Time = struct
  include Time_float_unix

  let now ~time_source =
    match time_source with
    | Some time_source ->
      Async_kernel.Synchronous_time_source.now time_source
      |> Core.Time_ns.to_time_float_round_nearest
    | None -> now ()
  ;;
end

module Synchronous_time_source = struct
  include Async_kernel.Synchronous_time_source

  let default =
    if Ppx_inline_test_lib.am_running
    then read_only (create ~now:Core.Time_ns.epoch ())
    else wall_clock ()
  ;;
end

include struct
  open Async_unix
  module Reader = Reader
  module Shutdown = Shutdown
  module Sys = Sys
  module Unix = Unix
  module Writer = Writer
end
