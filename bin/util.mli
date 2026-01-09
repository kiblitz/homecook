open! Core
open! Import

module Constants : sig
  val board : string
end

module Resources : sig
  module Piece : sig
    val svg : Homecook_lib.Piece.t -> string
  end
end

val maybe_attr : bool -> attr:Vdom.Attr.t -> Vdom.Attr.t
