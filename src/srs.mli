open! Core

(** Spaced-repetition scheduler for a single "card" (one position where the
    trainee must recall one move).

    The scheduler is a simplified SM-2 variant that folds the two practice
    signals homecook cares about — how long recall took and how many tries it
    needed — into a grade, and is robust to the user walking away mid-card:
    recall durations are capped (see {!cap_recall_ms}) so an idle tab can never
    push a card's interval to absurd values.

    All timestamps and durations are milliseconds ([float]) so the module stays
    free of any time/browser dependency and remains deterministic under test. *)

module Grade : sig
  type t =
    | Again (** failed / had to peek *)
    | Hard (** correct but slow or effortful *)
    | Good (** correct *)
    | Easy (** correct and immediate *)
  [@@deriving equal, sexp, enumerate]
end

module Performance : sig
  (** Raw signals captured during one practice attempt of a card. *)
  type t =
    { tries : int (** attempts before the correct move was played (>= 1) *)
    ; recall_ms : float (** wall-clock from prompt to correct move *)
    }
  [@@deriving equal, sexp]
end

type t [@@deriving equal, sexp]

(** A fresh card that has never been reviewed; due immediately. *)
val create : t

(** Longest recall duration (ms) that counts toward grading. Anything longer is
    assumed to include time away from the board and is clamped to this. *)
val afk_cap_ms : float

(** Clamp a raw recall duration into [0, afk_cap_ms]. *)
val cap_recall_ms : float -> float

(** Turn raw practice signals into a grade, applying the AFK cap. *)
val grade_of_performance : Performance.t -> Grade.t

val is_due : t -> now:float -> bool
val due : t -> float option
val interval_days : t -> float
val ease : t -> float
val reps : t -> int
val lapses : t -> int

(** Advance the schedule after a graded review performed at [now] (epoch ms). *)
val review : t -> Grade.t -> now:float -> t

(** [review] straight from raw performance signals. *)
val review_performance : t -> Performance.t -> now:float -> t
