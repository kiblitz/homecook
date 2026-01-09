open! Core

type t [@@deriving equal, sexp_of]

module type Ruleset = sig
  val default : t
  val valid_squares : t -> source:Square.t -> Square.Set.t
end

val valid_squares : ?ruleset:(module Ruleset) -> t -> source:Square.t -> Square.Set.t
val pieces : t -> Piece.t Square.Map.t
val to_move : t -> Color.t
val move : ?ruleset:(module Ruleset) -> t -> from:Square.t -> to_:Square.t -> t option

module Standard : Ruleset
