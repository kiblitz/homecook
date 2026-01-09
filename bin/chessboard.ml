open! Core
open! Import

module Stylesheet =
  [%css
    stylesheet
      {|
    .board {
      display: inline-grid;
      grid-template-columns: repeat(8, 5rem);
      grid-template-rows: repeat(8, 5rem);
      border: 0.3vmin solid #333;
    }

    .light-square {
      position: relative;
      background-color: #f0d9b5;
    }

    .dark-square {
      position: relative;
      background-color: #b58863;
    }

    .selected-square {
      box-shadow: inset 0 0 0 0.3vmin #f6f669;
    }

    .possible-move-dot {
      position: absolute;
      width: 30%;
      height: 30%;
      top: 0; left: 0; right: 0; bottom: 0;
      background: rgba(0, 0, 0, 0.15);
      border-radius: 50%;
      margin: auto;
    }

    .capturable-frame {
      position: absolute;
      top: 0; left: 0; right: 0; bottom: 0;
      border: 8px solid rgba(0, 0, 0, 0.15);
      border-radius: 50%;
    }
  |}]

module State = struct
  type t =
    { chessboard : Homecook_lib.Chessboard.t
    ; piece_drag_square : Square.t option
    ; hover_square : Square.t option
    }
  [@@deriving equal, sexp_of]

  module Action = struct
    type t =
      | Start_piece_drag of Square.t
      | Set_piece_square of Square.t
      | Hover_square of Square.t
      | Unhover_square
    [@@deriving sexp_of]
  end

  let default =
    { chessboard = Homecook_lib.Chessboard.Standard.default
    ; piece_drag_square = None
    ; hover_square = None
    }
  ;;

  let bonsai ?(default = default) graph =
    Bonsai.state_machine0
      ~default_model:default
      ~sexp_of_model:[%sexp_of: t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~equal
      ~apply_action:(fun (_ : Action.t Bonsai.Apply_action_context.t) t -> function
         | Start_piece_drag square -> { t with piece_drag_square = Some square }
         | Set_piece_square new_square ->
           let updated_chessboard =
             let%bind.Option piece_drag_square = t.piece_drag_square in
             Homecook_lib.Chessboard.move
               t.chessboard
               ~move:
                 { Homecook_lib.Chessboard.Move.source = piece_drag_square
                 ; target = new_square
                 }
           in
           let t = { t with piece_drag_square = None } in
           (match updated_chessboard with
            | None -> t
            | Some updated_chessboard -> { t with chessboard = updated_chessboard })
         | Hover_square new_square -> { t with hover_square = Some new_square }
         | Unhover_square -> { t with hover_square = None })
      graph
  ;;
end

let create ~width ~height ~create_square graph =
  let files =
    List.range 0 width
    |> List.map ~f:(File.of_idx >> ok_exn)
    |> File.Set.of_list
    |> return
  in
  let ranks =
    List.range 0 height
    |> List.map ~f:(Rank.of_idx >> ok_exn)
    |> Rank.Set.of_list
    |> return
  in
  Bonsai.assoc_set
    (module File)
    files
    graph
    ~f:(fun file graph ->
      Bonsai.assoc_set
        (module Rank)
        ranks
        graph
        ~f:(fun rank graph -> create_square ~rank ~file ~graph))
;;

let piece_img ~(state : State.t) ~(set_state : State.Action.t -> unit Effect.t) ~square =
  let%map.Option piece =
    Map.find (Homecook_lib.Chessboard.pieces state.chessboard) square
  in
  let drag_attrs =
    let draggable =
      [%equal: Homecook_lib.Color.t]
        piece.color
        (Homecook_lib.Chessboard.to_move state.chessboard)
    in
    Vdom.Attr.draggable draggable
    ::
    (if not draggable
     then []
     else
       [ Vdom.Attr.on_dragstart (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
           Effect.all_unit
             [ set_state (Start_piece_drag square); set_state (Hover_square square) ])
       ; Vdom.Attr.on_dragend (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
           let hover_square = state.hover_square in
           let%bind.Effect () = set_state Unhover_square in
           match hover_square with
           | None -> Effect.Ignore
           | Some hover_square -> set_state (Set_piece_square hover_square))
       ])
  in
  Vdom.Node.img
    ~attrs:
      ([ Vdom.Attr.src (Util.Resources.Piece.svg piece)
       ; [ Css_gen.width (`Percent Percent.one_hundred_percent)
         ; Css_gen.height (`Percent Percent.one_hundred_percent)
         ; Css_gen.user_select `None
         ]
         |> Css_gen.concat
         |> Vdom.Attr.style
       ]
       @ drag_attrs)
    ()
;;

let component ?(width = 8) ?(height = 8) graph =
  let state, set_state = State.bonsai graph in
  let valid_squares =
    let%arr { chessboard; piece_drag_square; hover_square = _ } = state in
    match piece_drag_square with
    | None -> Square.Set.empty
    | Some piece_drag_square ->
      Homecook_lib.Chessboard.valid_squares chessboard ~source:piece_drag_square
  in
  let create_square ~rank ~file ~graph:_ =
    let%arr rank = rank
    and file = file
    and state = state
    and set_state = set_state
    and valid_squares = valid_squares in
    let square = { Square.file; rank } in
    let is_valid_square = Set.mem valid_squares square in
    let is_capturable_square =
      is_valid_square && Map.mem (Homecook_lib.Chessboard.pieces state.chessboard) square
    in
    let is_light =
      ((Rank.to_idx rank |> ok_exn) + (File.to_idx file |> ok_exn)) % 2 = 1
    in
    let is_selected = Option.exists state.hover_square ~f:([%equal: Square.t] square) in
    let piece_img = piece_img ~state ~set_state ~square in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.create "coord" (Square.to_string square)
        ; (if is_light then Stylesheet.light_square else Stylesheet.dark_square)
        ; Util.maybe_attr is_selected ~attr:Stylesheet.selected_square
        ; Vdom.Attr.on_dragenter (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
            if Option.is_none state.piece_drag_square
            then Effect.Ignore
            else Effect.all_unit [ set_state (Hover_square square) ])
        ; Vdom.Attr.on_dragover (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
            Effect.Prevent_default)
        ; [ Css_gen.create
              ~field:"grid-row"
              ~value:[%string "%{height - (Rank.to_idx rank |> ok_exn)#Int}"]
          ; Css_gen.create
              ~field:"grid-column"
              ~value:[%string "%{(File.to_idx file |> ok_exn) + 1#Int}"]
          ]
          |> Css_gen.concat
          |> Vdom.Attr.style
        ]
      ([ piece_img
       ; Option.some_if
           is_valid_square
           (Vdom.Node.div
              ~attrs:
                [ Util.maybe_attr
                    (is_valid_square && not is_capturable_square)
                    ~attr:Stylesheet.possible_move_dot
                ; Util.maybe_attr is_capturable_square ~attr:Stylesheet.capturable_frame
                ]
              [])
       ]
       |> List.filter_opt)
  in
  let%arr grid = create ~width ~height ~create_square graph
  and set_state = set_state in
  let squares = Map.data grid |> List.map ~f:Map.data |> List.concat in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.class_ Util.Constants.board
      ; Vdom.Attr.on_dragleave (fun (event : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
          let still_in_board =
            (let%bind.Option related_target = Js.Optdef.to_option event##.relatedTarget in
             let%bind.Option related_target = Js.Opt.to_option related_target in
             related_target##closest (Js.string [%string ".%{Util.Constants.board}"])
             |> Js.Opt.to_option)
            |> Option.is_some
          in
          if still_in_board then Ui_effect.Ignore else set_state Unhover_square)
      ; [ Css_gen.display `Inline_grid
        ; Css_gen.create
            ~field:"grid-template-columns"
            ~value:[%string "repeat(%{width#Int},5rem)"]
        ; Css_gen.create
            ~field:"grid-template-rows"
            ~value:[%string "repeat(%{height#Int},5rem)"]
        ; Css_gen.border ~width:(`Px 2) ~color:(`Hex "#333") ~style:`Solid ()
        ]
        |> Css_gen.concat
        |> Vdom.Attr.style
      ]
    squares
;;
