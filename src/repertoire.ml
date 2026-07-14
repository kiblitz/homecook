open! Core

module Move = Chessboard.Move

module Node = struct
  type t =
    { move : Move.t
    ; children : t list
    ; srs : Srs.t
    ; comment : string option
    }
  [@@deriving equal, sexp]
end

module Card = struct
  type t =
    { prompt : Move.t list
    ; answer : Move.t
    ; srs : Srs.t
    }
  [@@deriving equal, sexp]

  let line { prompt; answer; srs = _ } = prompt @ [ answer ]
end

type t =
  { name : string
  ; trainee : Color.t
  ; children : Node.t list
  }
[@@deriving equal, sexp]

let create ~name ~trainee = { name; trainee; children = [] }

(* Merge [moves] into a forest, sharing existing prefixes and appending new
   nodes (preserving order so the tree is stable across saves). *)
let rec add_moves nodes moves =
  match moves with
  | [] -> nodes
  | move :: rest ->
    if List.exists nodes ~f:(fun n -> Move.equal n.Node.move move)
    then
      List.map nodes ~f:(fun n ->
        if Move.equal n.Node.move move
        then { n with Node.children = add_moves n.Node.children rest }
        else n)
    else
      nodes
      @ [ { Node.move; children = add_moves [] rest; srs = Srs.create; comment = None } ]
;;

let add_line t moves = { t with children = add_moves t.children moves }

let cards t =
  let trainee_white = Color.equal t.trainee Color.White in
  let rec walk nodes ~ply ~prompt_rev acc =
    List.fold nodes ~init:acc ~f:(fun acc (node : Node.t) ->
      let is_trainee = Bool.equal (ply % 2 = 0) trainee_white in
      let acc =
        if is_trainee
        then
          { Card.prompt = List.rev prompt_rev; answer = node.move; srs = node.srs }
          :: acc
        else acc
      in
      walk node.children ~ply:(ply + 1) ~prompt_rev:(node.move :: prompt_rev) acc)
  in
  List.rev (walk t.children ~ply:0 ~prompt_rev:[] [])
;;

let card_count t = List.length (cards t)
let due_cards t ~now = List.filter (cards t) ~f:(fun c -> Srs.is_due c.Card.srs ~now)

let update_srs t ~line ~f =
  let rec go nodes moves =
    match moves with
    | [] -> nodes
    | [ last ] ->
      List.map nodes ~f:(fun n ->
        if Move.equal n.Node.move last then { n with Node.srs = f n.Node.srs } else n)
    | move :: rest ->
      List.map nodes ~f:(fun n ->
        if Move.equal n.Node.move move
        then { n with Node.children = go n.Node.children rest }
        else n)
  in
  { t with children = go t.children line }
;;

let%test_module _ =
  (module struct
    let sq s =
      let file = File.of_char (Char.uppercase s.[0]) |> ok_exn in
      let rank = Rank.of_int (Char.to_int s.[1] - Char.to_int '0') |> ok_exn in
      { Square.file; rank }
    ;;

    let mv a b = { Move.source = sq a; target = sq b }

    (* Italian and Ruy Lopez share 1.e4 e5 2.Nf3 Nc6 and diverge on White's 3rd. *)
    let italian = [ mv "E2" "E4"; mv "E7" "E5"; mv "G1" "F3"; mv "B8" "C6"; mv "F1" "C4" ]
    let ruy = [ mv "E2" "E4"; mv "E7" "E5"; mv "G1" "F3"; mv "B8" "C6"; mv "F1" "B5" ]

    let repertoire =
      create ~name:"e4 e5" ~trainee:White |> Fn.flip add_line italian |> Fn.flip add_line ruy
    ;;

    let%expect_test "shared prefix merges; trainee cards counted" =
      (* White cards: e4, Nf3, Bc4, Bb5 = 4. The shared e4/Nf3 are not doubled. *)
      printf "cards=%d\n" (card_count repertoire);
      [%expect {| cards=4 |}];
      printf "due=%d\n" (List.length (due_cards repertoire ~now:0.));
      [%expect {| due=4 |}]
    ;;

    let%expect_test "the top of the tree is a single shared e4 node" =
      printf "roots=%d\n" (List.length repertoire.children);
      [%expect {| roots=1 |}]
    ;;

    let%expect_test "update_srs targets exactly the addressed node" =
      let reviewed = update_srs repertoire ~line:italian ~f:(fun s -> Srs.review s Good ~now:0.) in
      let bc4_reps c =
        if List.equal Move.equal (Card.line c) italian then Some (Srs.reps c.Card.srs) else None
      in
      let reps = List.filter_map (cards reviewed) ~f:bc4_reps in
      print_s [%sexp (reps : int list)];
      [%expect {| (1) |}];
      (* Other cards remain new (0 reps). *)
      let total_reps = List.sum (module Int) (cards reviewed) ~f:(fun c -> Srs.reps c.Card.srs) in
      printf "total_reps=%d\n" total_reps;
      [%expect {| total_reps=1 |}]
    ;;
  end)
;;
