open! Core
open! Import

module Naming_scheme : sig
  type t =
    [ `Numbered
    | `Timestamped
    | `Dated
    | `User_defined of (module Rotation_id.S)
    ]
end

type t =
  { messages : int option
  ; size : Byte_units.t option
  ; time : Time.Ofday.t option
  ; keep : [ `All | `Newer_than of Time.Span.t | `At_least of int ]
  ; naming_scheme : Naming_scheme.t
  ; zone : Time.Zone.t
  }
[@@deriving fields ~getters, sexp_of]

val create
  :  ?messages:int
  -> ?size:Byte_units.t
  -> ?time:Time.Ofday.t
  -> ?zone:Time.Zone.t
  -> keep:[ `All | `Newer_than of Time.Span.t | `At_least of int ]
  -> naming_scheme:Naming_scheme.t
  -> unit
  -> t

val default : ?zone:Time.Zone.t -> unit -> t

val should_rotate
  :  t
  -> last_messages:int
  -> last_size:Byte_units.t
  -> last_time:Time_float.t
  -> current_time:Time_float.t
  -> bool
