open! Core

type t =
  | King
  | Queen
  | Rook
  | Bishop
  | Knight
  | Pawn
[@@deriving to_string]
