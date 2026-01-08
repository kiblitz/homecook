open! Core

type t = { pieces : Piece.t Square.Map.t } [@@deriving equal, sexp_of]

val standard : t
