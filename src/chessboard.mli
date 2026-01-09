open! Core

type t [@@deriving equal, sexp_of]

module type Ruleset = sig
  val default : t
  val valid_squares : t -> source:Square.t -> Square.Set.t
end

module Move : sig
  type t =
    { source : Square.t
    ; target : Square.t
    }
  [@@deriving equal, sexp_of]
end

val valid_squares : ?ruleset:(module Ruleset) -> t -> source:Square.t -> Square.Set.t
val pieces : t -> Piece.t Square.Map.t
val history_stack : t -> Move.t list
val to_move : t -> Color.t
val move : ?ruleset:(module Ruleset) -> t -> move:Move.t -> t option
val undo : t -> t Or_error.t

module Standard : Ruleset
