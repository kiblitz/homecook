open! Core

type t [@@deriving equal, sexp_of]

val pieces : t -> Piece.t Square.Map.t
val to_move : t -> Color.t
val move : t -> from:Square.t -> to_:Square.t -> t option
val standard : t
