open! Core
open! Import

type t =
  { time : Time.t
  ; level : Level.t option
  ; raw_message : [ Sexp_or_string.t | `Structured of Message_sexp.t ]
      (* [`Sexp] comes from uses of [%log.sexp] or from upstream logs, and [`String] comes
     from uses of [%log.string]; [`Sexp | `String] can also come from message events
     reconstructed out of serialized [Message]s.

     [`Structured] comes from uses of [%log]. *)
  ; source : Message_source.t
  ; legacy_tags : (string * string) list
  ; is_from_upstream_log : bool
  }
[@@deriving fields ~getters ~iterators:create]

let create ?time ?(source = "") ?(legacy_tags = []) ?level raw_message =
  let time = Option.value_or_thunk time ~default:(fun () -> Time.now ~time_source:None) in
  { raw_message
  ; source = Manually_constructed source
  ; level
  ; time
  ; legacy_tags
  ; is_from_upstream_log = false
  }
;;

let to_serialized_message_lossy
  { raw_message; source = _; level; time; legacy_tags; is_from_upstream_log = _ }
  =
  (match raw_message with
   | #Sexp_or_string.t as s -> s
   | `Structured data -> `Sexp (Message_sexp.render data))
  |> Message.create ?level ~time ~tags:legacy_tags
;;

let of_serialized_message msg =
  { raw_message =
      (Message.raw_message msg :> [ Sexp_or_string.t | `Structured of Message_sexp.t ])
  ; source = Manually_constructed "from serialized message"
  ; level = Message.level msg
  ; time = Message.time msg
  ; legacy_tags = Message.tags msg
  ; is_from_upstream_log = false
  }
;;

module Unstable = struct
  type nonrec t = t =
    { time : Time.t
    ; level : Level.t option [@sexp.option]
    ; raw_message :
        [ Sexp_or_string.Stable.V1.t | `Structured of Message_sexp.Unstable.t ]
    ; source : Message_source.t
    ; legacy_tags : (string * string) list [@sexp.list]
    ; is_from_upstream_log : bool [@sexp.bool]
    }
  [@@deriving sexp_of]
end

module Private = struct
  let create raw_message source = Fields.create ~raw_message ~source
  let is_from_upstream_log = is_from_upstream_log
end
