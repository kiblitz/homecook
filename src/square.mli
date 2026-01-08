open! Core

type t =
  { file : File.t
  ; rank : Rank.t
  }
[@@deriving compare, equal, sexp_of, to_string]

module Delta : sig
  type t =
    { file : int
    ; rank : int
    }
end

val validate : ?file_upper_bound:char -> ?rank_upper_bound:int -> t -> t Or_error.t
val ( + ) : t -> Delta.t -> t Or_error.t

include Comparable.S_plain with type t := t
