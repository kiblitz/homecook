open! Core
open! Import

module File = struct
  module T : sig
    type t [@@deriving compare, sexp_of]

    val to_string : t -> string
    val of_idx_exn : int -> t
    val of_char_exn : char -> t
    val to_idx : t -> int
  end = struct
    type t = char [@@deriving compare, sexp_of]

    let to_string = Char.to_string
    let is_valid_char c = Char.('A' <= c && c <= 'Z')

    let of_idx_exn i =
      let alpha_adjusted_i = i + Char.to_int 'A' in
      match Char.of_int alpha_adjusted_i with
      | Some c when is_valid_char c -> c
      | Some c -> raise_s [%message "max 26 files" (c : char)]
      | None ->
        raise_s [%message "char encoding out of range" (i : int) (alpha_adjusted_i : int)]
    ;;

    let to_idx t = Char.to_int t - Char.to_int 'A'

    let of_char_exn t =
      if is_valid_char t
      then t
      else raise_s [%message "Invalid char, expected capital alpha" (t : char)]
    ;;
  end

  include T
  include Comparable.Make_plain (T)
end

module Rank = struct
  module T : sig
    type t [@@deriving compare, sexp_of]

    val to_string : t -> string
    val of_idx : int -> t
    val to_idx : t -> int
    val of_int : int -> t
  end = struct
    type t = int [@@deriving compare, sexp_of]

    let to_string = Int.to_string
    let of_idx i = i + 1
    let to_idx t = t - 1
    let of_int = Fn.id
  end

  include T
  include Comparable.Make_plain (T)
end

module Square = struct
  type t =
    { file : File.t
    ; rank : Rank.t
    }
  [@@deriving equal, sexp_of]

  let to_string { file; rank } = [%string "%{file#File}%{rank#Rank}"]
end

module Stylesheet =
  [%css
    stylesheet
      {|
    .board {
      display: inline-grid;
      grid-template-columns: repeat(8, 5rem);
      grid-template-rows: repeat(8, 5rem);
      border: 2px solid #333;
    }

    .light-square { background-color: #eeeed2; }
    .dark-square { background-color: #769656; }

    .circle {
      width: 100%;
      height: 100%;
      border-radius: 50%;
      background-color: rgba(0, 0, 0, 0.5);
    }
  |}]

module State = struct
  type t =
    { piece_square : Square.t
    ; hover_square : Square.t option
    }
  [@@deriving equal, sexp_of]

  module Action = struct
    type t =
      | Set_piece_square of Square.t
      | Hover_square of Square.t
      | Unhover_square
    [@@deriving sexp_of]
  end

  let default =
    let file = File.of_char_exn 'E' in
    let rank = Rank.of_int 4 in
    { piece_square = { file; rank }; hover_square = None }
  ;;

  let bonsai ?(default = default) graph =
    Bonsai.state_machine0
      ~default_model:default
      ~sexp_of_model:[%sexp_of: t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~equal
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) t -> function
         | Set_piece_square new_square -> { t with piece_square = new_square }
         | Hover_square new_square -> { t with hover_square = Some new_square }
         | Unhover_square -> { t with hover_square = None })
      graph
  ;;
end

let create ~width ~height ~create_square graph =
  let files =
    List.range 0 width |> List.map ~f:File.of_idx_exn |> File.Set.of_list |> return
  in
  let ranks =
    List.range 0 height |> List.map ~f:Rank.of_idx |> Rank.Set.of_list |> return
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

let component ?(width = 8) ?(height = 8) graph =
  let state, set_state = State.bonsai graph in
  let create_square ~rank ~file ~graph:_ =
    let%arr rank = rank
    and file = file
    and state = state
    and set_state = set_state in
    let square = { Square.file; rank } in
    let is_light = (Rank.to_idx rank + File.to_idx file) % 2 = 0 in
    let piece =
      if [%equal: Square.t] square state.piece_square
      then
        Some
          (Vdom.Node.img
             ~attrs:
               [ Vdom.Attr.src (Util.Resources.Piece.svg White Bishop)
               ; [ Css_gen.width (`Percent Percent.one_hundred_percent)
                 ; Css_gen.height (`Percent Percent.one_hundred_percent)
                 ]
                 |> Css_gen.concat
                 |> Vdom.Attr.style
               ; Vdom.Attr.draggable true
               ; Vdom.Attr.on_dragstart (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
                   Effect.all_unit [ set_state (Hover_square square) ])
               ; Vdom.Attr.on_dragend (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
                   match state.hover_square with
                   | None -> set_state Unhover_square
                   | Some hover_square ->
                     Effect.all_unit
                       [ set_state (Set_piece_square hover_square)
                       ; set_state Unhover_square
                       ])
               ]
             ())
      else None
    in
    Vdom.Node.div
      ~attrs:
        [ (if is_light then Stylesheet.light_square else Stylesheet.dark_square)
        ; Vdom.Attr.create "coord" (Square.to_string square)
        ; Vdom.Attr.on_dragenter (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
            Effect.all_unit [ set_state (Hover_square square) ])
        ; Vdom.Attr.on_dragover (fun (_ : Js_of_ocaml.Dom_html.dragEvent Js.t) ->
            Effect.Prevent_default)
        ; [ Css_gen.create
              ~field:"grid-row"
              ~value:[%string "%{height - Rank.to_idx rank#Int}"]
          ; Css_gen.create
              ~field:"grid-column"
              ~value:[%string "%{File.to_idx file + 1#Int}"]
          ]
          |> Css_gen.concat
          |> Vdom.Attr.style
        ]
      (Option.to_list piece)
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
