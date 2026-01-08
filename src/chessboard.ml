open! Core

type t =
  { pieces : Piece.t Square.Map.t
  ; to_move : Color.t
  }
[@@deriving equal, fields ~getters, sexp_of]

let is_legal { pieces = _; to_move } ~from ~to_ ~(piece : Piece.t) =
  let different_square = not ([%equal: Square.t] from to_) in
  let piece_color_to_move = [%equal: Color.t] piece.color to_move in
  List.for_all [ different_square; piece_color_to_move ] ~f:Fn.id
;;

let move ({ pieces; to_move } as t) ~from ~to_ =
  let%bind.Option piece = Map.find pieces from in
  if not (is_legal t ~from ~to_ ~piece)
  then None
  else (
    let to_move =
      match to_move with
      | White -> Color.Black
      | Black -> Color.White
    in
    let pieces = Map.remove pieces from |> Map.set ~key:to_ ~data:piece in
    Some { pieces; to_move })
;;

let add ?(ranks = 8) file white_rank kind pieces =
  let white_piece = Piece.create ~color:White ~kind in
  let black_piece = Piece.create ~color:Black ~kind in
  let white_square =
    { Square.file = File.of_char_exn file; rank = Rank.of_int_exn white_rank }
  in
  let black_square =
    let black_rank = ranks - white_rank + 1 in
    { Square.file = File.of_char_exn file; rank = Rank.of_int_exn black_rank }
  in
  pieces
  |> Map.set ~key:white_square ~data:white_piece
  |> Map.set ~key:black_square ~data:black_piece
;;

let standard =
  let pieces =
    Square.Map.empty
    |> add 'A' 1 Rook
    |> add 'B' 1 Knight
    |> add 'C' 1 Bishop
    |> add 'D' 1 Queen
    |> add 'E' 1 King
    |> add 'F' 1 Bishop
    |> add 'G' 1 Knight
    |> add 'H' 1 Rook
    |> add 'A' 2 Pawn
    |> add 'B' 2 Pawn
    |> add 'C' 2 Pawn
    |> add 'D' 2 Pawn
    |> add 'E' 2 Pawn
    |> add 'F' 2 Pawn
    |> add 'G' 2 Pawn
    |> add 'H' 2 Pawn
  in
  { pieces; to_move = White }
;;
