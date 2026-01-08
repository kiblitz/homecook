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
      |> List.map ~f:(fun d_file ->
        next_square t ~source ~d_square:{ file = d_file; rank = d_rank })
      |> List.filter_map ~f:Next_square_result.to_option
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

  let valid_squares t ~source =
    (let%map.Option piece = Map.find t.pieces source in
     match (piece.kind : Piece_kind.t) with
     | Pawn -> valid_pawn_squares t ~source
     | Knight -> valid_knight_squares t ~source
     | _ (* TODO *) -> [])
    |> Option.value ~default:[]
    |> Square.Set.of_list
  ;;
end

let is_legal ({ pieces = _; to_move = _ } as t) ~(ruleset : (module Ruleset)) ~from ~to_ =
  let module Ruleset = (val ruleset) in
  let different_square = not ([%equal: Square.t] from to_) in
  let is_valid_square = Set.mem (Ruleset.valid_squares t ~source:from) to_ in
  List.for_all [ different_square; is_valid_square ] ~f:Fn.id
;;

let move ?(ruleset = (module Standard : Ruleset)) ({ pieces; to_move } as t) ~from ~to_ =
  let%bind.Option piece = Map.find pieces from in
  if (not ([%equal: Color.t] piece.color to_move)) || not (is_legal t ~ruleset ~from ~to_)
  then None
  else (
    let to_move = Color.swap to_move in
    let pieces = Map.remove pieces from |> Map.set ~key:to_ ~data:piece in
    Some { pieces; to_move })
;;
