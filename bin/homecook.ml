open! Core
open! Import
module Reorderable_list = Bonsai_web_ui_reorderable_list
module Form = Bonsai_web_ui_form.With_automatic_view

module Stylesheet =
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
  |}]

let item ~index:_ ~source _which _data graph =
  let text, set_text = Bonsai.state_opt ~equal:[%equal: string] graph in
  let%arr text = text
  and set_text = set_text
  and source = source in
  let view =
    View.hbox
      ~attrs:[ Stylesheet.item; source ]
      [ Vdom_input_widgets.Entry.text
          ~extra_attrs:[ Stylesheet.text_input ]
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
      ~extra_item_attrs:(return Stylesheet.transition_transform)
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
          ~attrs:[ Stylesheet.list ]
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
  Bonsai_web.Start.start Chessboard.component
;;
