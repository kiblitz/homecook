open! Core

type t =
  { file : File.t
  ; rank : Rank.t
  }
[@@deriving compare, equal, sexp_of, to_string]

include Comparable.S_plain with type t := t
