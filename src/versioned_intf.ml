open! Core
open! Import

module type S = sig
  type version
  type t [@@deriving bin_io, sexp]

  val version : version
end

module type Versioned = sig
  module Stable : sig
    module Version : sig
      type t = V2 [@@deriving bin_io, sexp, compare]

      val ( <> ) : t -> t -> bool
      val to_string : t -> string
    end

    module Make (T : S with type version := Version.t) : sig
      type t = T.t

      include Binable.S with type t := T.t
      include Sexpable.S with type t := T.t
    end
  end
end
