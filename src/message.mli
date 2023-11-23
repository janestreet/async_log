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

val time : t -> Time.t
val level : t -> Level.t option
val set_level : t -> Level.t option -> t
val message : t -> string
val raw_message : t -> [ `String of string | `Sexp of Sexp.t ]
val tags : t -> (string * string) list
val add_tags : t -> (string * string) list -> t
val to_write_only_text : ?zone:Time.Zone.t -> t -> string

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, sexp]

    module For_testing : sig
      val sexp_of_t_as_v0 : t -> Sexp.t
    end
  end
end
