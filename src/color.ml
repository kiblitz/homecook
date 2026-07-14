open! Core

type t =
  | White
  | Black
[@@deriving equal, sexp, to_string]

let swap = function
  | White -> Black
  | Black -> White
;;
