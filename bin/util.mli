open! Core
open! Import

module Constants : sig
  val board : string
end

module Resources : sig
  module Piece : sig
    val svg : Homecook_lib.Color.t -> Homecook_lib.Piece_kind.t -> string
  end
end
