include struct
  open Homecook_lib
  module File = File
  module Piece = Piece
  module Piece_kind = Piece_kind
  module Rank = Rank
  module Square = Square
end

include struct
  open Js_of_ocaml
  module Js = Js
end

include Bonsai_web.Cont
include Bonsai.Let_syntax
