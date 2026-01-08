open! Core

type t [@@deriving compare, sexp_of, to_string]

val validate : ?upper_bound:char -> t -> t Or_error.t
val of_idx : int -> t Or_error.t
val to_idx : t -> int Or_error.t
val of_char : char -> t Or_error.t
val to_char : t -> char Or_error.t
val ( + ) : t -> int -> t Or_error.t

include Comparable.S_plain with type t := t
