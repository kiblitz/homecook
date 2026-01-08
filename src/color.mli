open! Core

type t =
  | White
  | Black
[@@deriving equal, sexp_of, to_string]
