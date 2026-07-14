open! Core
open! Import
module Repertoire = Homecook_lib.Repertoire
module Card = Homecook_lib.Repertoire.Card
module Color = Homecook_lib.Color
module Home_cook = Homecook_lib.Home_cook
module Position = Homecook_lib.Chessboard
module Move = Homecook_lib.Chessboard.Move
module Srs = Homecook_lib.Srs

module Style =
  [%css
    stylesheet
      {|
    .app { font-family: sans-serif; color: #eee; min-height: 100vh; }
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
    .rep-list { display: flex; flex-direction: column; gap: 0.6rem; max-width: 640px; }
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
    .row { display: flex; gap: 0.5rem; align-items: center; }
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
    .practice {
      max-width: 560px;
      margin: 0 auto;
      display: flex;
      flex-direction: column;
      gap: 0.75rem;
      align-items: center;
    }
    .prompt { font-size: 1.05rem; }
    .feedback { min-height: 1.4rem; color: #c9a26b; font-weight: 600; }
    .progress { color: #9a938c; font-size: 0.85rem; }
    .controls { display: flex; gap: 0.75rem; align-items: center; flex-wrap: wrap; justify-content: center; }
    .input {
      background: #2b2927;
      color: #eee;
      border: 1px solid #555;
      border-radius: 6px;
      padding: 0.4rem 0.6rem;
      font-size: 1rem;
    }
    .moves {
      display: flex;
      flex-wrap: wrap;
      gap: 0.3rem;
      max-width: 560px;
      justify-content: center;
      min-height: 1.2rem;
    }
    .movechip {
      background: #3b3835;
      border-radius: 4px;
      padding: 0.15rem 0.4rem;
      font-size: 0.85rem;
      color: #ddd;
    }
  |}]

let now_ms t = Time_ns.Span.to_ms (Time_ns.to_span_since_epoch t)

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

let grade_label : Srs.Grade.t -> string = function
  | Again -> "Again"
  | Hard -> "Hard"
  | Good -> "Good"
  | Easy -> "Easy"
;;

module Mode = struct
  type t =
    | Home
    | Board
  [@@deriving equal, sexp]
end

(* An in-progress practice run over one repertoire's due cards. [queue]'s head is
   the current card; the rest are still to come. *)
module Session = struct
  type t =
    { rep_index : int
    ; queue : Card.t list
    ; selected : Square.t option (* tap-to-move source *)
    ; tries : int
    ; revealed : bool
    ; answered : bool (* current card graded; waiting for Next *)
    ; reviewed : int
    ; feedback : string option
    }
  [@@deriving equal, sexp]
end

(* Building or modifying an opening by playing moves on the board. [lines] holds
   already-committed variants; [line] is the one currently being played. *)
module Editor = struct
  type t =
    { name : string
    ; trainee : Color.t
    ; lines : Move.t list list
    ; line : Move.t list
    ; selected : Square.t option
    }
  [@@deriving equal, sexp]

  let empty =
    { name = ""; trainee = Color.White; lines = []; line = []; selected = None }
  ;;

  (* Every line the editor would save: the committed ones plus the current one. *)
  let all_lines t = t.lines @ if List.is_empty t.line then [] else [ t.line ]

  let to_repertoire t =
    let name = if String.is_empty (String.strip t.name) then "Untitled opening" else t.name in
    List.fold
      (all_lines t)
      ~init:(Repertoire.create ~name ~trainee:t.trainee)
      ~f:Repertoire.add_line
  ;;
end

module Model = struct
  type t =
    { mode : Mode.t
    ; repertoires : Repertoire.t list
    ; practice : Session.t option
    ; editor : Editor.t option
    }
  [@@deriving equal, sexp]

  let default =
    { mode = Mode.Home; repertoires = []; practice = None; editor = None }
  ;;

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
    | Start_practice of int * float (* rep index, now (epoch ms) *)
    | P_click of Square.t * float
    | P_reveal of float
    | P_next
    | Exit_practice
    | Start_editor
    | E_click of Square.t
    | E_undo
    | E_commit_line
    | E_set_name of string
    | E_toggle_trainee
    | E_save
    | Exit_editor
  [@@deriving sexp_of]
end

(* Apply [grade] to the current card's schedule within repertoire [rep_index]. *)
let grade_card repertoires ~rep_index ~card ~grade ~now =
  List.mapi repertoires ~f:(fun i rep ->
    if i = rep_index
    then
      Repertoire.update_srs rep ~line:(Card.line card) ~f:(fun srs ->
        Srs.review srs grade ~now)
    else rep)
;;

let state_machine graph =
  Bonsai.state_machine0
    ~default_model:(Model.initial ())
    ~sexp_of_model:[%sexp_of: Model.t]
    ~sexp_of_action:[%sexp_of: Action.t]
    ~equal:[%equal: Model.t]
    ~apply_action:(fun ctx (model : Model.t) action ->
      let persist repertoires =
        Bonsai.Apply_action_context.schedule_event
          ctx
          (Storage.save (Home_cook.create repertoires))
      in
      match (action : Action.t) with
      | Set_mode mode -> { model with mode }
      | Add_sample ->
        let repertoires = model.repertoires @ [ sample_repertoire () ] in
        persist repertoires;
        { model with repertoires }
      | Remove i ->
        let repertoires = List.filteri model.repertoires ~f:(fun j _ -> j <> i) in
        persist repertoires;
        { model with repertoires; practice = None }
      | Exit_practice -> { model with practice = None }
      | Start_practice (i, now) ->
        (match List.nth model.repertoires i with
         | None -> model
         | Some rep ->
           let queue = Repertoire.due_cards rep ~now in
           { model with
             practice =
               Some
                 { rep_index = i
                 ; queue
                 ; selected = None
                 ; tries = 0
                 ; revealed = false
                 ; answered = false
                 ; reviewed = 0
                 ; feedback = None
                 }
           })
      | P_next ->
        (match model.practice with
         | None -> model
         | Some session ->
           let queue =
             match session.queue with
             | [] -> []
             | _ :: rest -> rest
           in
           { model with
             practice =
               Some
                 { session with
                   queue
                 ; selected = None
                 ; tries = 0
                 ; revealed = false
                 ; answered = false
                 ; reviewed = session.reviewed + 1
                 ; feedback = None
                 }
           })
      | P_reveal now ->
        (match model.practice with
         | None -> model
         | Some session ->
           (match session.queue with
            | [] -> model
            | card :: _ when not session.answered ->
              let grade = Practice.grade_of_tries ~revealed:true ~tries:(session.tries + 1) in
              let repertoires =
                grade_card model.repertoires ~rep_index:session.rep_index ~card ~grade ~now
              in
              persist repertoires;
              { model with
                repertoires
              ; practice =
                  Some
                    { session with
                      revealed = true
                    ; answered = true
                    ; selected = None
                    ; feedback = Some "Revealed — marked for review."
                    }
              }
            | _ -> model))
      | P_click (square, now) ->
        (match model.practice with
         | None -> model
         | Some session ->
           (match session.queue with
            | [] -> model
            | _ :: _ when session.answered -> model
            | card :: _ ->
              (match session.selected with
               | None ->
                 let position = Practice.position_after card.prompt in
                 (match Map.find (Position.pieces position) square with
                  | Some piece when Color.equal piece.color (Position.to_move position) ->
                    { model with
                      practice = Some { session with selected = Some square; feedback = None }
                    }
                  | _ -> model)
               | Some source ->
                 if [%equal: Square.t] source square
                 then { model with practice = Some { session with selected = None } }
                 else (
                   let attempt = { Move.source; target = square } in
                   if Move.equal attempt card.answer
                   then (
                     let grade =
                       Practice.grade_of_tries ~revealed:session.revealed ~tries:(session.tries + 1)
                     in
                     let repertoires =
                       grade_card model.repertoires ~rep_index:session.rep_index ~card ~grade ~now
                     in
                     persist repertoires;
                     { model with
                       repertoires
                     ; practice =
                         Some
                           { session with
                             answered = true
                           ; selected = None
                           ; feedback = Some [%string "Correct! (%{grade_label grade})"]
                           }
                     })
                   else
                     { model with
                       practice =
                         Some
                           { session with
                             selected = None
                           ; tries = session.tries + 1
                           ; feedback = Some "Not that move — try again."
                           }
                     })))))
      | Start_editor -> { model with editor = Some Editor.empty }
      | Exit_editor -> { model with editor = None }
      | E_set_name name ->
        (match model.editor with
         | None -> model
         | Some editor -> { model with editor = Some { editor with name } })
      | E_toggle_trainee ->
        (match model.editor with
         | None -> model
         | Some editor ->
           { model with editor = Some { editor with trainee = Color.swap editor.trainee } })
      | E_undo ->
        (match model.editor with
         | None -> model
         | Some editor ->
           let line = Option.value (List.drop_last editor.line) ~default:[] in
           { model with editor = Some { editor with line; selected = None } })
      | E_commit_line ->
        (match model.editor with
         | None -> model
         | Some editor ->
           if List.is_empty editor.line
           then model
           else
             { model with
               editor =
                 Some
                   { editor with
                     lines = editor.lines @ [ editor.line ]
                   ; line = []
                   ; selected = None
                   }
             })
      | E_save ->
        (match model.editor with
         | None -> model
         | Some editor ->
           if List.is_empty (Editor.all_lines editor)
           then { model with editor = None; mode = Mode.Home }
           else (
             let repertoires = model.repertoires @ [ Editor.to_repertoire editor ] in
             persist repertoires;
             { model with repertoires; editor = None; mode = Mode.Home }))
      | E_click square ->
        (match model.editor with
         | None -> model
         | Some editor ->
           let position = Practice.position_after editor.line in
           (match editor.selected with
            | None ->
              (match Map.find (Position.pieces position) square with
               | Some piece when Color.equal piece.color (Position.to_move position) ->
                 { model with editor = Some { editor with selected = Some square } }
               | _ -> model)
            | Some source ->
              if [%equal: Square.t] source square
              then { model with editor = Some { editor with selected = None } }
              else (
                let move = { Move.source; target = square } in
                match Position.move position ~move with
                | Some _ ->
                  { model with
                    editor = Some { editor with line = editor.line @ [ move ]; selected = None }
                  }
                | None -> { model with editor = Some { editor with selected = None } }))))
    graph
;;

let tab ~label ~active ~on_click =
  Vdom.Node.button
    ~attrs:
      [ Style.tab
      ; Util.maybe_attr active ~attr:Style.tab_active
      ; Vdom.Attr.on_click (fun _ -> on_click)
      ]
    [ Vdom.Node.text label ]
;;

let home_view ~(model : Model.t) ~inject ~now =
  let rows =
    List.mapi model.repertoires ~f:(fun i r ->
      let cards = Repertoire.card_count r in
      let due = List.length (Repertoire.due_cards r ~now) in
      Vdom.Node.div
        ~attrs:[ Style.rep_card ]
        [ Vdom.Node.div
            [ Vdom.Node.div ~attrs:[ Style.rep_name ] [ Vdom.Node.text r.name ]
            ; Vdom.Node.div
                ~attrs:[ Style.rep_stats ]
                [ Vdom.Node.text [%string "%{cards#Int} moves · %{due#Int} due"] ]
            ]
        ; Vdom.Node.div
            ~attrs:[ Style.row ]
            [ Vdom.Node.button
                ~attrs:
                  [ Style.btn
                  ; Vdom.Attr.on_click (fun _ ->
                      inject (Action.Start_practice (i, now)))
                  ]
                [ Vdom.Node.text "Practice" ]
            ; Vdom.Node.button
                ~attrs:
                  [ Style.btn_ghost
                  ; Vdom.Attr.on_click (fun _ -> inject (Action.Remove i))
                  ]
                [ Vdom.Node.text "Delete" ]
            ]
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
         @ [ Vdom.Node.div
               ~attrs:[ Style.controls ]
               [ Vdom.Node.button
                   ~attrs:[ Style.btn; Vdom.Attr.on_click (fun _ -> inject Action.Start_editor) ]
                   [ Vdom.Node.text "＋ New opening" ]
               ; Vdom.Node.button
                   ~attrs:
                     [ Style.btn_ghost
                     ; Vdom.Attr.on_click (fun _ -> inject Action.Add_sample)
                     ]
                   [ Vdom.Node.text "Add sample: 1.e4 e5" ]
               ]
           ])
    ]
;;

let practice_view ~(session : Session.t) ~inject ~now =
  let header =
    Vdom.Node.div
      ~attrs:[ Style.header ]
      [ Vdom.Node.button
          ~attrs:[ Style.tab; Vdom.Attr.on_click (fun _ -> inject Action.Exit_practice) ]
          [ Vdom.Node.text "← Back" ]
      ; Vdom.Node.div ~attrs:[ Style.spacer ] []
      ; Vdom.Node.div
          ~attrs:[ Style.progress ]
          [ Vdom.Node.text
              [%string
                "reviewed %{session.reviewed#Int} · %{List.length session.queue#Int} left"]
          ]
      ]
  in
  let content =
    match session.queue with
    | [] ->
      Vdom.Node.div
        ~attrs:[ Style.practice ]
        [ Vdom.Node.h2 [ Vdom.Node.text "Session complete 🎉" ]
        ; Vdom.Node.div
            ~attrs:[ Style.progress ]
            [ Vdom.Node.text [%string "%{session.reviewed#Int} cards reviewed"] ]
        ; Vdom.Node.button
            ~attrs:[ Style.btn; Vdom.Attr.on_click (fun _ -> inject Action.Exit_practice) ]
            [ Vdom.Node.text "Done" ]
        ]
    | card :: _ ->
      let position = Practice.position_after card.prompt in
      let hint =
        if session.revealed || session.answered
        then Some (card.answer.source, card.answer.target)
        else None
      in
      let board =
        Practice.render_board
          ~position
          ~selected:session.selected
          ~hint
          ~on_click:(fun square -> inject (Action.P_click (square, now)))
      in
      let to_move = Position.to_move position in
      let controls =
        if session.answered
        then
          [ Vdom.Node.button
              ~attrs:[ Style.btn; Vdom.Attr.on_click (fun _ -> inject Action.P_next) ]
              [ Vdom.Node.text "Next →" ]
          ]
        else
          [ Vdom.Node.button
              ~attrs:
                [ Style.btn_ghost
                ; Vdom.Attr.on_click (fun _ -> inject (Action.P_reveal now))
                ]
              [ Vdom.Node.text "Show answer" ]
          ; Vdom.Node.div
              ~attrs:[ Style.progress ]
              [ Vdom.Node.text [%string "tries: %{session.tries#Int}"] ]
          ]
      in
      Vdom.Node.div
        ~attrs:[ Style.practice ]
        [ Vdom.Node.div
            ~attrs:[ Style.prompt ]
            [ Vdom.Node.text
                [%string "%{Color.to_string to_move} to move — play your prepared move"]
            ]
        ; board
        ; Vdom.Node.div
            ~attrs:[ Style.feedback ]
            [ Vdom.Node.text (Option.value session.feedback ~default:"") ]
        ; Vdom.Node.div ~attrs:[ Style.controls ] controls
        ]
  in
  Vdom.Node.div ~attrs:[ Style.app ] [ header; Vdom.Node.div ~attrs:[ Style.body ] [ content ] ]
;;

let editor_view ~(editor : Editor.t) ~inject =
  let position = Practice.position_after editor.line in
  let board =
    Practice.render_board
      ~position
      ~selected:editor.selected
      ~hint:None
      ~on_click:(fun square -> inject (Action.E_click square))
  in
  let move_chip m =
    Vdom.Node.div
      ~attrs:[ Style.movechip ]
      [ Vdom.Node.text [%string "%{m.Move.source#Square}%{m.Move.target#Square}"] ]
  in
  let header =
    Vdom.Node.div
      ~attrs:[ Style.header ]
      [ Vdom.Node.button
          ~attrs:[ Style.tab; Vdom.Attr.on_click (fun _ -> inject Action.Exit_editor) ]
          [ Vdom.Node.text "← Cancel" ]
      ; Vdom.Node.div ~attrs:[ Style.spacer ] []
      ; Vdom.Node.button
          ~attrs:[ Style.btn; Vdom.Attr.on_click (fun _ -> inject Action.E_save) ]
          [ Vdom.Node.text "Save opening" ]
      ]
  in
  let to_move = Position.to_move position in
  let content =
    Vdom.Node.div
      ~attrs:[ Style.practice ]
      [ Vdom.Node.input
          ~attrs:
            [ Style.input
            ; Vdom.Attr.type_ "text"
            ; Vdom.Attr.string_property "value" editor.name
            ; Vdom.Attr.placeholder "Opening name"
            ; Vdom.Attr.on_input (fun _ value -> inject (Action.E_set_name value))
            ]
          ()
      ; Vdom.Node.div
          ~attrs:[ Style.controls ]
          [ Vdom.Node.button
              ~attrs:[ Style.tab; Vdom.Attr.on_click (fun _ -> inject Action.E_toggle_trainee) ]
              [ Vdom.Node.text [%string "Training as: %{Color.to_string editor.trainee}"] ]
          ; Vdom.Node.div
              ~attrs:[ Style.progress ]
              [ Vdom.Node.text
                  [%string "%{List.length editor.lines#Int} variant(s) saved"]
              ]
          ]
      ; Vdom.Node.div
          ~attrs:[ Style.prompt ]
          [ Vdom.Node.text
              [%string "%{Color.to_string to_move} to move — tap to add the next move"]
          ]
      ; board
      ; Vdom.Node.div ~attrs:[ Style.moves ] (List.map editor.line ~f:move_chip)
      ; Vdom.Node.div
          ~attrs:[ Style.controls ]
          [ Vdom.Node.button
              ~attrs:[ Style.btn_ghost; Vdom.Attr.on_click (fun _ -> inject Action.E_undo) ]
              [ Vdom.Node.text "Undo move" ]
          ; Vdom.Node.button
              ~attrs:[ Style.btn_ghost; Vdom.Attr.on_click (fun _ -> inject Action.E_commit_line) ]
              [ Vdom.Node.text "Start another variant" ]
          ]
      ]
  in
  Vdom.Node.div ~attrs:[ Style.app ] [ header; Vdom.Node.div ~attrs:[ Style.body ] [ content ] ]
;;

let component graph =
  let state, inject = state_machine graph in
  let board = Chessboard.component graph in
  let clock = Bonsai.Clock.now graph in
  let%arr (model : Model.t) = state
  and inject = inject
  and board = board
  and clock = clock in
  let now = now_ms clock in
  match model.editor, model.practice with
  | Some editor, _ -> editor_view ~editor ~inject
  | None, Some session -> practice_view ~session ~inject ~now
  | None, None ->
    let body =
      match model.mode with
      | Mode.Home -> home_view ~model ~inject ~now
      | Mode.Board ->
        Vdom.Node.div
          ~attrs:[ Style.body ]
          [ Vdom.Node.div ~attrs:[ Style.board_wrap ] [ board ] ]
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
