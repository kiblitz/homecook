open! Core
open! Import
module Repertoire = Homecook_lib.Repertoire
module Color = Homecook_lib.Color
module Home_cook = Homecook_lib.Home_cook
module Move = Homecook_lib.Chessboard.Move

module Style =
  [%css
    stylesheet
      {|
    .app {
      font-family: sans-serif;
      color: #eee;
      min-height: 100vh;
    }
    .header {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
      gap: 0.5rem;
      padding: 0.75rem 1rem;
      background: #3b3835;
      border-bottom: 1px solid #000;
    }
    .title { font-weight: 700; font-size: 1.15rem; margin-right: 1rem; }
    .tab {
      background: #454241;
      color: #eee;
      border: none;
      padding: 0.4rem 0.9rem;
      border-radius: 6px;
      cursor: pointer;
      font-size: 0.95rem;
    }
    .tab-active { background: #c9a26b; color: #1c1a19; }
    .spacer { flex: 1 1 auto; }
    .body { padding: 1rem; }
    .rep-list {
      display: flex;
      flex-direction: column;
      gap: 0.6rem;
      max-width: 640px;
    }
    .rep-card {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 0.75rem;
      background: #454241;
      border-radius: 8px;
      padding: 0.75rem 1rem;
    }
    .rep-name { font-weight: 600; }
    .rep-stats { color: #c9a26b; font-size: 0.85rem; }
    .btn {
      background: #c9a26b;
      color: #1c1a19;
      border: none;
      padding: 0.5rem 0.9rem;
      border-radius: 6px;
      cursor: pointer;
      font-weight: 600;
    }
    .btn-ghost {
      background: transparent;
      color: #e0a0a0;
      border: 1px solid #744;
      padding: 0.35rem 0.6rem;
      border-radius: 6px;
      cursor: pointer;
    }
    .empty { color: #9a938c; margin-bottom: 1rem; }
    .board-wrap { display: flex; justify-content: center; padding-top: 0.5rem; }
  |}]

(* Build a move from algebraic squares like "E2" "E4". *)
let sq s =
  let file = File.of_char (Char.uppercase s.[0]) |> ok_exn in
  let rank = Rank.of_int (Char.to_int s.[1] - Char.to_int '0') |> ok_exn in
  { Square.file; rank }
;;

let mv a b = { Move.source = sq a; target = sq b }

(* A ready-made repertoire so the list has something to show before the editor
   exists: 1.e4 e5 2.Nf3 Nc6 then both the Italian (3.Bc4) and Ruy Lopez
   (3.Bb5). *)
let sample_repertoire () =
  let common = [ mv "E2" "E4"; mv "E7" "E5"; mv "G1" "F3"; mv "B8" "C6" ] in
  Repertoire.create ~name:"1.e4 e5 (Italian + Ruy Lopez)" ~trainee:Color.White
  |> Fn.flip Repertoire.add_line (common @ [ mv "F1" "C4" ])
  |> Fn.flip Repertoire.add_line (common @ [ mv "F1" "B5" ])
;;

module Mode = struct
  type t =
    | Home
    | Board
  [@@deriving equal, sexp]
end

module Model = struct
  type t =
    { mode : Mode.t
    ; repertoires : Repertoire.t list
    }
  [@@deriving equal, sexp]

  let default = { mode = Mode.Home; repertoires = [] }

  (* Rehydrate from browser storage so a reload keeps the user's home cook. *)
  let initial () =
    match Storage.load () with
    | Some home_cook -> { default with repertoires = home_cook.repertoires }
    | None -> default
  ;;
end

module Action = struct
  type t =
    | Set_mode of Mode.t
    | Add_sample
    | Remove of int
  [@@deriving sexp_of]
end

let state_machine graph =
  Bonsai.state_machine0
    ~default_model:(Model.initial ())
    ~sexp_of_model:[%sexp_of: Model.t]
    ~sexp_of_action:[%sexp_of: Action.t]
    ~equal:[%equal: Model.t]
    ~apply_action:(fun ctx (model : Model.t) action ->
      (* Mirror every repertoire change to browser storage. *)
      let persist (model : Model.t) =
        Bonsai.Apply_action_context.schedule_event
          ctx
          (Storage.save (Home_cook.create model.repertoires));
        model
      in
      match (action : Action.t) with
      | Set_mode mode -> { model with mode }
      | Add_sample ->
        persist { model with repertoires = model.repertoires @ [ sample_repertoire () ] }
      | Remove i ->
        persist
          { model with repertoires = List.filteri model.repertoires ~f:(fun j _ -> j <> i) })
    graph
;;

let tab ~label ~active ~on_click =
  Vdom.Node.button
    ~attrs:[ Style.tab; Util.maybe_attr active ~attr:Style.tab_active; Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text label ]
;;

let home_view ~(model : Model.t) ~inject =
  let rows =
    List.mapi model.repertoires ~f:(fun i r ->
      let cards = Repertoire.card_count r in
      let due = List.length (Repertoire.due_cards r ~now:0.) in
      Vdom.Node.div
        ~attrs:[ Style.rep_card ]
        [ Vdom.Node.div
            [ Vdom.Node.div ~attrs:[ Style.rep_name ] [ Vdom.Node.text r.name ]
            ; Vdom.Node.div
                ~attrs:[ Style.rep_stats ]
                [ Vdom.Node.text [%string "%{cards#Int} moves · %{due#Int} due"] ]
            ]
        ; Vdom.Node.button
            ~attrs:[ Style.btn_ghost; Vdom.Attr.on_click (fun _ -> inject (Action.Remove i)) ]
            [ Vdom.Node.text "Delete" ]
        ])
  in
  let empty =
    if List.is_empty model.repertoires
    then
      [ Vdom.Node.div
          ~attrs:[ Style.empty ]
          [ Vdom.Node.text "No openings yet. Add one to start cooking." ]
      ]
    else []
  in
  Vdom.Node.div
    ~attrs:[ Style.body ]
    [ Vdom.Node.div
        ~attrs:[ Style.rep_list ]
        (empty
         @ rows
         @ [ Vdom.Node.button
               ~attrs:[ Style.btn; Vdom.Attr.on_click (fun _ -> inject Action.Add_sample) ]
               [ Vdom.Node.text "＋ Add sample: 1.e4 e5" ]
           ])
    ]
;;

let component graph =
  let state, inject = state_machine graph in
  let board = Chessboard.component graph in
  let%arr (model : Model.t) = state
  and inject = inject
  and board = board in
  let body =
    match model.mode with
    | Mode.Home -> home_view ~model ~inject
    | Mode.Board ->
      Vdom.Node.div ~attrs:[ Style.body ] [ Vdom.Node.div ~attrs:[ Style.board_wrap ] [ board ] ]
  in
  Vdom.Node.div
    ~attrs:[ Style.app ]
    [ Vdom.Node.div
        ~attrs:[ Style.header ]
        [ Vdom.Node.div ~attrs:[ Style.title ] [ Vdom.Node.text "🍳 homecook" ]
        ; tab
            ~label:"Openings"
            ~active:(Mode.equal model.mode Mode.Home)
            ~on_click:(inject (Action.Set_mode Mode.Home))
        ; tab
            ~label:"Board"
            ~active:(Mode.equal model.mode Mode.Board)
            ~on_click:(inject (Action.Set_mode Mode.Board))
        ; Vdom.Node.div ~attrs:[ Style.spacer ] []
        ; Vdom.Node.button
            ~attrs:
              [ Style.tab
              ; Vdom.Attr.on_click (fun _ ->
                  Storage.download (Home_cook.create model.repertoires))
              ]
            [ Vdom.Node.text "⬇ Download" ]
        ]
    ; body
    ]
;;
