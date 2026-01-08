open! Core

type t =
  { color : Color.t
  ; kind : Piece_kind.t
  }
[@@deriving equal, sexp_of]

val create : color:Color.t -> kind:Piece_kind.t -> t
