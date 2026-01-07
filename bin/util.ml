open! Core
open! Import

module Constants = struct
  let board = "board"
end

module Resources = struct
  module Piece = struct
    let svg color kind =
      let color = Homecook_lib.Color.to_string color |> String.lowercase in
      let kind = Homecook_lib.Piece_kind.to_string kind |> String.lowercase in
      [%string "%{color}_%{kind}.svg"]
    ;;
  end
end
