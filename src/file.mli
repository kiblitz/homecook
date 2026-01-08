open! Core

type t [@@deriving compare, sexp_of, to_string]

val of_idx_exn : int -> t
val to_idx : t -> int
val of_char_exn : char -> t
val to_char : t -> char

include Comparable.S_plain with type t := t
