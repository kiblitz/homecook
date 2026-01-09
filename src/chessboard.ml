open! Core

type t =
  { pieces : Piece.t Square.Map.t
  ; to_move : Color.t
  }
[@@deriving equal, fields ~getters, sexp_of]

module type Ruleset = sig
  val default : t
  val valid_squares : t -> source:Square.t -> Square.Set.t
end

let rank_flip ?(ranks = 8) rank = ranks - rank + 1

let rank_flip_if_black ?ranks rank ~color =
  match (color : Color.t) with
  | White -> rank
  | Black -> rank_flip ?ranks rank
;;

let add ?ranks file white_rank kind pieces =
  let white_piece = Piece.create ~color:White ~kind in
  let black_piece = Piece.create ~color:Black ~kind in
  let square color =
    let rank = rank_flip_if_black ?ranks white_rank ~color in
    { Square.file = File.of_char file |> ok_exn; rank = Rank.of_int rank |> ok_exn }
  in
  pieces
  |> Map.set ~key:(square White) ~data:white_piece
  |> Map.set ~key:(square Black) ~data:black_piece
;;

module Next_square_result = struct
  type t =
    | Some of Square.t
    | Taken of Square.t
    | None

  let to_option t : Square.t option =
    match t with
    | Some square | Taken square -> Some square
    | None -> None
  ;;
end

let next_square ?(can_take = true) { pieces; to_move } ~source ~d_square
  : Next_square_result.t
  =
  let next_square =
    (let%bind.Or_error next_square = Square.(source + d_square) in
     Square.validate ~file_upper_bound:'H' ~rank_upper_bound:8 next_square)
    |> Or_error.ok
  in
  match next_square with
  | None -> None
  | Some next_square ->
    (match Map.find pieces next_square with
     | None -> Some next_square
     | Some piece ->
       if (not ([%equal: Color.t] piece.color to_move)) && can_take
       then Taken next_square
       else None)
;;

let next_squares ?(can_take = true) t ~source ~d_squares =
  List.folding_map
    d_squares
    ~init:(Next_square_result.Some source)
    ~f:(fun source d_square ->
      let next_square =
        match source with
        | Some source -> next_square ~can_take t ~source ~d_square
        | None | Taken (_ : Square.t) -> None
      in
      next_square, next_square)
  |> List.filter_map ~f:Next_square_result.to_option
;;

module Standard : Ruleset = struct
  let default =
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

  let valid_pawn_squares t ~(source : Square.t) =
    let starting_rank = rank_flip_if_black 2 ~color:t.to_move |> Rank.of_int |> ok_exn in
    let d_rank =
      match t.to_move with
      | White -> 1
      | Black -> -1
    in
    let forward =
      let d_squares =
        let d_square d_rank = { Square.Delta.file = 0; rank = d_rank } in
        if [%equal: Rank.t] source.rank starting_rank
        then [ d_square d_rank; d_square d_rank ]
        else [ d_square d_rank ]
      in
      next_squares ~can_take:false t ~source ~d_squares
    in
    let capture =
      [ -1; 1 ]
      |> List.filter_map ~f:(fun d_file ->
        let d_square = { Square.Delta.file = d_file; rank = d_rank } in
        let%bind.Option new_square = Or_error.ok Square.(source + d_square) in
        let%bind.Option maybe_piece_to_capture = Map.find t.pieces new_square in
        if not ([%equal: Color.t] maybe_piece_to_capture.color t.to_move)
        then Some new_square
        else None)
    in
    capture @ forward
  ;;

  let valid_knight_squares t ~(source : Square.t) =
    let d_squares =
      let%bind.List abs_d_file = [ 1; 2 ] in
      let%bind.List d_file = [ -abs_d_file; abs_d_file ] in
      let abs_d_rank = if abs_d_file = 1 then 2 else 1 in
      let%map.List d_rank = [ -abs_d_rank; abs_d_rank ] in
      { Square.Delta.file = d_file; rank = d_rank }
    in
    d_squares
    |> List.map ~f:(fun d_square -> next_square t ~source ~d_square)
    |> List.filter_map ~f:Next_square_result.to_option
  ;;

  let valid_bishop_squares t ~(source : Square.t) =
    let d_squares =
      let%map.List d_file = [ -1; 1 ]
      and d_rank = [ -1; 1 ] in
      let d_square = { Square.Delta.file = d_file; rank = d_rank } in
      List.init 7 ~f:(Fn.const d_square)
    in
    List.concat_map d_squares ~f:(fun d_squares -> next_squares t ~source ~d_squares)
  ;;

  let valid_rook_squares t ~(source : Square.t) =
    let d_squares =
      let%bind.List d = [ -1; 1 ] in
      let d_square_rank = { Square.Delta.file = 0; rank = d } in
      let d_square_file = { Square.Delta.file = d; rank = 0 } in
      let%map.List d_square = [ d_square_rank; d_square_file ] in
      List.init 7 ~f:(Fn.const d_square)
    in
    List.concat_map d_squares ~f:(fun d_squares -> next_squares t ~source ~d_squares)
  ;;

  let valid_king_squares t ~(source : Square.t) =
    let d_squares =
      let d = [ -1; 0; 1 ] in
      let%bind.List d_file = d
      and d_rank = d in
      if d_rank = 0 && d_file = 0
      then []
      else [ { Square.Delta.file = d_file; rank = d_rank } ]
    in
    d_squares
    |> List.map ~f:(fun d_square -> next_square t ~source ~d_square)
    |> List.filter_map ~f:Next_square_result.to_option
  ;;

  let valid_squares t ~source =
    (let%bind.Option piece = Map.find t.pieces source in
     if not ([%equal: Color.t] piece.color t.to_move)
     then None
     else
       (match (piece.kind : Piece_kind.t) with
        | Pawn -> valid_pawn_squares t ~source
        | Knight -> valid_knight_squares t ~source
        | Bishop -> valid_bishop_squares t ~source
        | Rook -> valid_rook_squares t ~source
        | Queen -> valid_bishop_squares t ~source @ valid_rook_squares t ~source
        | King -> valid_king_squares t ~source)
       |> Some)
    |> Option.value ~default:[]
    |> Square.Set.of_list
    |> Fn.flip Set.remove source
  ;;
end

let valid_squares ?(ruleset = (module Standard : Ruleset)) t ~source =
  let module Ruleset = (val ruleset) in
  Ruleset.valid_squares t ~source
;;

let is_legal ?ruleset t ~from ~to_ = Set.mem (valid_squares ?ruleset t ~source:from) to_

let move ?(ruleset = (module Standard : Ruleset)) ({ pieces; to_move } as t) ~from ~to_ =
  let%bind.Option piece = Map.find pieces from in
  if not (is_legal t ~ruleset ~from ~to_)
  then None
  else (
    let to_move = Color.swap to_move in
    let pieces = Map.remove pieces from |> Map.set ~key:to_ ~data:piece in
    Some { pieces; to_move })
;;
