open! Core

type t =
  | White
  | Black
[@@deriving equal, sexp, to_string]

val swap : t -> t
