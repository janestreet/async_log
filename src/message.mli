open! Core
open! Import

type t [@@deriving sexp_of]

val create
  :  ?level:Level.t
  -> ?time:Time.t
  -> ?time_source:Synchronous_time_source.t
  -> ?tags:(string * string) list
  -> Sexp_or_string.t
  -> t

val create_from_upstream_log
  :  Sexp_or_string.t
  -> level:Level.t
  -> time:Time_float.t
  -> tags:(string * string) list
  -> t

val time : t -> Time.t
val level : t -> Level.t option
val set_level : t -> Level.t option -> t
val message : t -> string
val raw_message : t -> [ `String of string | `Sexp of Sexp.t ]
val tags : t -> (string * string) list
val add_tags : t -> (string * string) list -> t
val to_write_only_text : ?zone:Time.Zone.t -> t -> string
val is_from_upstream_log : t -> bool

module Stable : sig
  module Versioned : sig
    type nonrec t = t [@@deriving of_sexp]
  end

  module Version : sig
    type t [@@deriving of_sexp]
  end

  module V0 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end

  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end
end
