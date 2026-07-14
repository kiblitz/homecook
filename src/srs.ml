open! Core

module Grade = struct
  type t =
    | Again
    | Hard
    | Good
    | Easy
  [@@deriving equal, sexp, enumerate]
end

module Performance = struct
  type t =
    { tries : int
    ; recall_ms : float
    }
  [@@deriving equal, sexp]
end

type t =
  { ease : float
  ; interval_days : float
  ; reps : int (** consecutive non-Again reviews *)
  ; lapses : int
  ; due : float option (** epoch ms; [None] means new / due immediately *)
  ; last_reviewed : float option
  }
[@@deriving equal, sexp]

let ease t = t.ease
let interval_days t = t.interval_days
let reps t = t.reps
let lapses t = t.lapses
let due t = t.due

let ms_per_day = 86_400_000.
let ms_per_min = 60_000.
let afk_cap_ms = 60_000.
let min_ease = 1.3
let default_ease = 2.5

let create =
  { ease = default_ease
  ; interval_days = 0.
  ; reps = 0
  ; lapses = 0
  ; due = None
  ; last_reviewed = None
  }
;;

let cap_recall_ms recall_ms = Float.clamp_exn recall_ms ~min:0. ~max:afk_cap_ms

let grade_of_performance { Performance.tries; recall_ms } =
  let recall = cap_recall_ms recall_ms in
  if tries >= 3
  then Grade.Again
  else if tries = 2
  then Grade.Hard
  else if Float.( <= ) recall 3_000.
  then Grade.Easy
  else if Float.( <= ) recall 10_000.
  then Grade.Good
  else Grade.Hard
;;

let is_due t ~now =
  match t.due with
  | None -> true
  | Some due -> Float.( >= ) now due
;;

let ease_delta : Grade.t -> float = function
  | Again -> -0.20
  | Hard -> -0.15
  | Good -> 0.
  | Easy -> 0.15
;;

let review t (grade : Grade.t) ~now =
  let ease = Float.max min_ease (t.ease +. ease_delta grade) in
  let reps, interval_days, lapses =
    match grade with
    | Again -> 0, 0., t.lapses + 1
    | Hard ->
      let interval =
        if t.reps = 0 then 0.5 else Float.max 1. (t.interval_days *. 1.2)
      in
      t.reps + 1, interval, t.lapses
    | Good ->
      let interval =
        match t.reps with
        | 0 -> 1.
        | 1 -> 3.
        | _ -> t.interval_days *. ease
      in
      t.reps + 1, interval, t.lapses
    | Easy ->
      let interval =
        match t.reps with
        | 0 -> 2.
        | 1 -> 5.
        | _ -> t.interval_days *. ease *. 1.3
      in
      t.reps + 1, interval, t.lapses
  in
  let due =
    match grade with
    (* A lapse comes back this session rather than tomorrow. *)
    | Again -> now +. (10. *. ms_per_min)
    | Hard | Good | Easy -> now +. (interval_days *. ms_per_day)
  in
  { ease; interval_days; reps; lapses; due = Some due; last_reviewed = Some now }
;;

let review_performance t performance ~now =
  review t (grade_of_performance performance) ~now
;;

let%expect_test "new card is always due" =
  print_s [%sexp (is_due create ~now:0. : bool)];
  [%expect {| true |}]
;;

let%expect_test "grade from performance, with AFK cap" =
  let grade tries recall_ms =
    grade_of_performance { Performance.tries; recall_ms }
  in
  let show tries recall_ms = print_s [%sexp (grade tries recall_ms : Grade.t)] in
  show 1 2_000.;
  [%expect {| Easy |}];
  show 1 7_000.;
  [%expect {| Good |}];
  show 1 20_000.;
  [%expect {| Hard |}];
  show 2 1_000.;
  [%expect {| Hard |}];
  show 4 1_000.;
  [%expect {| Again |}];
  (* An AFK recall of ten minutes is clamped to the cap, so a first-try card
     grades no worse than Hard rather than poisoning the schedule. *)
  show 1 600_000.;
  [%expect {| Hard |}]
;;

let%expect_test "Good reviews grow the interval; ease is preserved" =
  let t = create in
  let show t = printf "interval=%.2f ease=%.2f\n" (interval_days t) (ease t) in
  let t = review t Good ~now:0. in
  show t;
  [%expect {| interval=1.00 ease=2.50 |}];
  let t = review t Good ~now:(interval_days t *. ms_per_day) in
  show t;
  [%expect {| interval=3.00 ease=2.50 |}];
  let t = review t Good ~now:0. in
  show t;
  [%expect {| interval=7.50 ease=2.50 |}]
;;

let%expect_test "Again lapses the card and re-shows it in ~10 minutes" =
  let t = review create Good ~now:0. in
  let t = review t Again ~now:0. in
  print_s [%sexp (reps t : int)];
  [%expect {| 0 |}];
  print_s [%sexp (lapses t : int)];
  [%expect {| 1 |}];
  print_s [%sexp (is_due t ~now:(5. *. ms_per_min) : bool)];
  [%expect {| false |}];
  print_s [%sexp (is_due t ~now:(11. *. ms_per_min) : bool)];
  [%expect {| true |}]
;;

let%expect_test "ease shrinks on Hard but never below the floor" =
  let t = ref create in
  for _ = 1 to 20 do
    t := review !t Hard ~now:0.
  done;
  printf "%.2f\n" (ease !t);
  [%expect {| 1.30 |}]
;;
