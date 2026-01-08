open! Core

type t =
  { color : Color.t
  ; kind : Piece_kind.t
  }
[@@deriving equal, sexp_of]

let create ~color ~kind = { color; kind }
