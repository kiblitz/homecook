open! Core

type t [@@deriving compare, sexp_of, to_string]

val validate : ?upper_bound:int -> t -> t Or_error.t
val of_idx : int -> t Or_error.t
val to_idx : t -> int Or_error.t
val of_int : int -> t Or_error.t
val to_int : t -> int Or_error.t
val ( + ) : t -> int -> t Or_error.t

include Comparable.S_plain with type t := t
