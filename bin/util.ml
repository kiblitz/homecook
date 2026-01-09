open! Core
open! Import

module Constants = struct
  let board = "board"
end

module Resources = struct
  module Piece = struct
    let svg { Homecook_lib.Piece.color; kind } =
      let color = Homecook_lib.Color.to_string color |> String.lowercase in
      let kind = Piece_kind.to_string kind |> String.lowercase in
      [%string "%{color}_%{kind}.svg"]
    ;;
  end
end

let maybe_attr condition ~attr = if condition then attr else Vdom.Attr.empty
