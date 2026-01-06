open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
module Reorderable_list = Bonsai_web_ui_reorderable_list
module Form = Bonsai_web_ui_form.With_automatic_view

module S =
  [%css
    stylesheet
      {|
      .item {
        background-color: green;
        color: white;
        font-size: 20px;
        padding: 5px;
        margin: 5px;
      }

      .text_input {
        flex: 1;
      }

      .list {
        flex: 1;
      }

      .transition_transform {
        transition: transform 0.3s, opacity 0.1s, background-color 0.3s;
      }

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

(*
let draggable_piece =
  Bonsai_web_ui_drag_and_drop.create
    ~source_id:(module Int)
    ~target_id:(module Int)
    ~on_drop:((fun _source_id _target_id -> Ui_effect.Ignore) |> return)
;;
*)

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

let chessboard_component ?(width = 8) ?(height = 8) graph =
  let files =
    List.range 0 width |> List.map ~f:File.of_idx_exn |> File.Set.of_list |> return
  in
  let ranks =
    List.range 0 height |> List.map ~f:Rank.of_idx |> Rank.Set.of_list |> return
  in
  let grid =
    Bonsai.assoc_set
      (module File)
      files
      graph
      ~f:(fun file graph ->
        Bonsai.assoc_set
          (module Rank)
          ranks
          graph
          ~f:(fun rank _graph ->
            let%arr rank = rank
            and file = file in
            let is_light = (Rank.to_idx rank + File.to_idx file) % 2 = 0 in
            let piece =
              if
                [%equal: File.t] file (File.of_char_exn 'E')
                && [%equal: Rank.t] rank (Rank.of_int 4)
              then Some (Vdom.Node.div ~attrs:[ S.circle; Vdom.Attr.draggable true ] [])
              else None
            in
            Vdom.Node.div
              ~attrs:
                [ (if is_light then S.light_square else S.dark_square)
                ; Vdom.Attr.create "coord" [%string "%{file#File}%{rank#Rank}"]
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
              (Option.to_list piece)))
  in
  let%arr grid = grid in
  let squares = Map.data grid |> List.map ~f:Map.data |> List.concat in
  Vdom.Node.div
    ~attrs:
      [ [ Css_gen.display `Inline_grid
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

let item ~index:_ ~source _which _data graph =
  let text, set_text = Bonsai.state_opt ~equal:[%equal: string] graph in
  let%arr text = text
  and set_text = set_text
  and source = source in
  let view =
    View.hbox
      ~attrs:[ S.item; source ]
      [ Vdom_input_widgets.Entry.text
          ~extra_attrs:[ S.text_input ]
          ~value:text
          ~on_input:set_text
          ~allow_updates_when_focused:`Never
          ()
      ]
  in
  (), view
;;

let component graph =
  let input, extend_input =
    Bonsai.state_machine0
      ~sexp_of_model:[%sexp_of: Int.Set.t]
      ~equal:[%equal: Int.Set.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:(Int.Set.of_list [ 0; 1; 2 ])
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model () ->
        Set.add model (Set.length model))
      graph
  in
  let () =
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:true
      (Time_ns.Span.of_sec 1.0)
      (let%arr extend_input = extend_input in
       extend_input ())
      graph
  in
  let num_lists =
    Form.Elements.Number.int
      ~default:1
      ~step:1
      ~allow_updates_when_focused:`Never
      ()
      graph
  in
  let whiches =
    let%arr num_lists = num_lists in
    let length = Int.max 0 (Form.value_or_default num_lists ~default:1) in
    Int.Set.of_list (List.range 0 length)
  in
  let reorderable_list =
    Reorderable_list.Multi.simple
      (module Int)
      (module Int)
      ~extra_item_attrs:(return S.transition_transform)
      ~default_item_height:40
      ~render:item
      ~lists:whiches
      ~default_list:(return 0)
      input
      graph
  in
  let lists =
    Bonsai.assoc
      (module Int)
      (let%arr lists, _ = reorderable_list in
       lists)
      ~f:(fun which data _graph ->
        let%arr _, view = data
        and which = which in
        Vdom.Node.div
          ~attrs:[ S.list ]
          [ Vdom.Node.h3 [ Vdom.Node.text [%string "List %{which#Int}"] ]; view ])
      graph
  in
  let%arr lists = lists
  and num_lists = num_lists
  and _, dragged_element = reorderable_list in
  Vdom.Node.div
    [ Form.view_as_vdom num_lists; View.hbox (Map.data lists); dragged_element ]
;;

let () =
  ignore component;
  Bonsai_web.Start.start chessboard_component
;;
