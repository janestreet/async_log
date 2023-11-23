open! Core
open! Async_kernel
open! Import
include Raw_log

(* This implementation file mainly includes helper functions for specific message formats.
   The core logging functionality, independent of format, etc., is in [Raw_log]. *)

let sexp_of_t (_ : t) = Sexp.Atom "<opaque>"

let message t msg =
  if would_log t (Message.level msg)
  then push_message_event t (Message_event.of_serialized_message msg)
;;

let push_message_event t data source ~level ~time ~legacy_tags =
  let time =
    Option.value_or_thunk time ~default:(fun () ->
      Synchronous_time_source.now (get_time_source t)
      |> Time_ns.to_time_float_round_nearest)
  in
  let legacy_tags = Option.value legacy_tags ~default:[] in
  Message_event.Private.create
    data
    source
    ~level
    ~time
    ~legacy_tags
    ~is_from_upstream_log:false
  |> push_message_event t
;;

let push_message t msg ~level ~time ~tags =
  push_message_event
    t
    msg
    (Manually_constructed "from async log")
    ~level
    ~time
    ~legacy_tags:tags
;;

let sexp ?level ?time ?tags t sexp =
  if would_log t level then push_message t (`Sexp sexp) ~level ~time ~tags
;;

let string ?level ?time ?tags t s =
  if would_log t level then push_message t (`String s) ~level ~time ~tags
;;

let structured_message ?level ?time ?tags t data source =
  if would_log t level
  then push_message_event t (`Structured data) source ~level ~time ~legacy_tags:tags
;;

let printf ?level ?time ?tags t fmt =
  if would_log t level
  then ksprintf (fun msg -> push_message t (`String msg) ~level ~time ~tags) fmt
  else ifprintf () fmt
;;

let add_uuid_to_tags tags =
  let uuid =
    match Base.Exported_for_specific_uses.am_testing with
    | true -> Uuid.Stable.V1.for_testing
    | false -> Uuid_unix.create ()
  in
  ("Log.surround_id", Uuid.to_string uuid) :: tags
;;

let surround_s_gen
  ?(tags = [])
  ~try_with
  ~map_return
  ~(log_sexp : ?tags:(string * string) list -> Sexp.t -> unit)
  ~f
  msg
  =
  let tags = add_uuid_to_tags tags in
  log_sexp ~tags [%message "Enter" ~_:(msg : Sexp.t)];
  map_return (try_with f) ~f:(function
    | Ok x ->
      log_sexp ~tags [%message "Exit" ~_:(msg : Sexp.t)];
      x
    | Error exn ->
      log_sexp ~tags [%message "Raised while " ~_:(msg : Sexp.t) (exn : exn)];
      Exn.reraise exn (sprintf !"%{sexp:Sexp.t}" msg))
;;

let surroundf_gen
  ?(tags = [])
  ~try_with
  ~map_return
  ~(log_string : ?tags:(string * string) list -> string -> unit)
  =
  ksprintf (fun msg f ->
    let tags = add_uuid_to_tags tags in
    log_string ~tags ("Enter " ^ msg);
    map_return (try_with f) ~f:(function
      | Ok x ->
        log_string ~tags ("Exit " ^ msg);
        x
      | Error exn ->
        log_string ~tags ("Raised while " ^ msg ^ ":" ^ Exn.to_string exn);
        Exn.reraise exn msg))
;;

let surround_s ~on_subsequent_errors ?level ?time ?tags t msg f =
  surround_s_gen
    ?tags
    ~try_with:(Monitor.try_with ~run:`Schedule ~rest:on_subsequent_errors)
    ~map_return:Deferred.map
    ~log_sexp:(fun ?tags s -> sexp ?tags ?level ?time t s)
    ~f
    msg
;;

let surroundf ~on_subsequent_errors ?level ?time ?tags t fmt =
  surroundf_gen
    ?tags
    ~try_with:(Monitor.try_with ~run:`Schedule ~rest:on_subsequent_errors)
    ~map_return:Deferred.map
    ~log_string:(fun ?tags -> string ?tags ?level ?time t)
    fmt
;;

let set_level_via_param_helper ~f =
  let open Command.Param in
  map
    (flag "log-level" (optional Level.arg) ~doc:"LEVEL The log level")
    ~f:(Option.iter ~f)
;;

let set_level_via_param log = set_level_via_param_helper ~f:(set_level log)

let set_level_via_param_lazy log =
  set_level_via_param_helper ~f:(fun level -> set_level (Lazy.force log) level)
;;

let raw ?time ?tags t fmt = printf ?time ?tags t fmt
let debug ?time ?tags t fmt = printf ~level:`Debug ?time ?tags t fmt
let info ?time ?tags t fmt = printf ~level:`Info ?time ?tags t fmt
let error ?time ?tags t fmt = printf ~level:`Error ?time ?tags t fmt
let raw_s ?time ?tags t the_sexp = sexp ?time ?tags t the_sexp
let debug_s ?time ?tags t the_sexp = sexp ~level:`Debug ?time ?tags t the_sexp
let info_s ?time ?tags t the_sexp = sexp ~level:`Info ?time ?tags t the_sexp
let error_s ?time ?tags t the_sexp = sexp ~level:`Error ?time ?tags t the_sexp

let%bench_module "unused log messages" =
  (module struct
    let log =
      create
        ~level:`Info
        ~output:[ Output_unix.file `Text ~filename:"/dev/null" ]
        ~on_error:`Raise
        ~time_source:None
        ~transform:None
    ;;

    let%bench "unused printf" = debug log "blah"
    let%bench "unused printf w/subst" = debug log "%s" "blah"
    let%bench "unused string" = string log ~level:`Debug "blah"
    let%bench "used printf" = info log "blah"
  end)
;;

module For_testing = struct
  let create_output = Output.For_testing.create

  let create ~map_output level =
    let output = [ create_output ~map_output ] in
    create ~output ~level ~on_error:`Raise ~time_source:None ~transform:None
  ;;
end

module Private = struct
  let push_upstream_log_sexp t sexp ~level ~time ~tags:legacy_tags =
    Message_event.Private.create
      (`Sexp sexp)
      (Manually_constructed "from upstream log")
      ~level:(Some level)
      ~time
      ~legacy_tags
      ~is_from_upstream_log:true
    |> Raw_log.push_message_event t
  ;;

  let set_level_via_param_lazy = set_level_via_param_lazy
end

let create ~level ~output ~on_error ?time_source ?transform () =
  create ~level ~output ~on_error ~time_source ~transform
;;

let create_null () =
  create ~level:`Error ~output:[] ~on_error:(`Call (fun (_ : Error.t) -> ())) ()
;;
