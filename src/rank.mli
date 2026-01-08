open! Core

type t [@@deriving compare, sexp_of]

val to_string : t -> string
val of_idx_exn : int -> t
val to_idx : t -> int
val of_int_exn : int -> t
val to_int : t -> int

include Comparable.S_plain with type t := t
