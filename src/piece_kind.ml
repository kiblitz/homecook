open! Core

type t =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
[@@deriving equal, sexp_of, to_string]
