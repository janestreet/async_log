open! Core
open! Import

type t =
  [ `Sexp of Sexp.t
  | `String of string
  ]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, sexp]

    val to_string : t -> string
  end
end
