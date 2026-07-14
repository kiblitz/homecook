open! Core
open! Import
module Chessboard = Homecook_lib.Chessboard
module Srs = Homecook_lib.Srs

module Style =
  [%css
    stylesheet
      {|
    .board {
      display: inline-grid;
      grid-template-columns: repeat(8, min(4.5rem, 11vmin));
      grid-template-rows: repeat(8, min(4.5rem, 11vmin));
      border: 0.3vmin solid #333;
      touch-action: manipulation;
    }
    .light { background-color: #f0d9b5; position: relative; }
    .dark { background-color: #b58863; position: relative; }
    .square { cursor: pointer; position: relative; }
    .selected { box-shadow: inset 0 0 0 0.4vmin #f6f669; }
    .hint { box-shadow: inset 0 0 0 0.4vmin #6fb1f6; }
    .piece { width: 100%; height: 100%; user-select: none; pointer-events: none; }
  |}]

(* Replay a line of moves from the start to get the position the trainee sees. *)
let position_after moves =
  List.fold moves ~init:Chessboard.Standard.default ~f:(fun board move ->
    Option.value (Chessboard.move board ~move) ~default:board)
;;

(* Grade a card from how it was answered. Recall-time weighting is a planned
   refinement; for now the number of tries (and whether the answer was revealed)
   drives the grade. *)
let grade_of_tries ~revealed ~tries : Srs.Grade.t =
  if revealed
  then Again
  else (
    match tries with
    | 1 -> Good
    | 2 -> Hard
    | _ -> Again)
;;

let piece_node position square =
  match Map.find (Chessboard.pieces position) square with
  | None -> []
  | Some piece ->
    [ Vdom.Node.img
        ~attrs:[ Style.piece; Vdom.Attr.src (Util.Resources.Piece.svg piece) ]
        ()
    ]
;;

(* A tap-to-move board: renders [position], highlights the selected source and
   an optional answer [hint], and reports each tapped square to [on_click]. *)
let render_board ~position ~selected ~hint ~on_click =
  let hint_squares =
    match hint with
    | None -> Square.Set.empty
    | Some (source, target) -> Square.Set.of_list [ source; target ]
  in
  let squares =
    let%bind.List rank_idx = List.range 0 8 |> List.rev in
    let%map.List file_idx = List.range 0 8 in
    let file = File.of_idx file_idx |> ok_exn in
    let rank = Rank.of_idx rank_idx |> ok_exn in
    let square = { Square.file; rank } in
    let is_light = (rank_idx + file_idx) % 2 = 1 in
    let is_selected = Option.exists selected ~f:([%equal: Square.t] square) in
    let is_hint = Set.mem hint_squares square in
    Vdom.Node.div
      ~attrs:
        [ Style.square
        ; (if is_light then Style.light else Style.dark)
        ; Util.maybe_attr is_selected ~attr:Style.selected
        ; Util.maybe_attr is_hint ~attr:Style.hint
        ; Vdom.Attr.create "coord" (Square.to_string square)
        ; Vdom.Attr.on_click (fun _ -> on_click square)
        ; [ Css_gen.create ~field:"grid-row" ~value:[%string "%{8 - rank_idx#Int}"]
          ; Css_gen.create ~field:"grid-column" ~value:[%string "%{file_idx + 1#Int}"]
          ]
          |> Css_gen.concat
          |> Vdom.Attr.style
        ]
      (piece_node position square)
  in
  Vdom.Node.div ~attrs:[ Style.board ] squares
;;
