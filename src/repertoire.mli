open! Core

(** An opening repertoire: a {e tree} of moves (so variants are first-class)
    that the trainee is memorizing for one side. Each node carries its own
    spaced-repetition state, so different lines are scheduled independently.

    Plies are counted from the initial position, where White moves first. A node
    is a {e card} — something the trainee must recall — when the side to move at
    that ply is [trainee]. *)

module Node : sig
  type t =
    { move : Chessboard.Move.t (** the move leading from the parent position to here *)
    ; children : t list (** prepared continuations (variants) *)
    ; srs : Srs.t (** schedule for recalling [move] (only meaningful for cards) *)
    ; comment : string option
    }
  [@@deriving equal, sexp]
end

module Card : sig
  (** One memorization target: play [answer] from the position reached by
      following [prompt] (moves from the start, not including [answer]). *)
  type t =
    { prompt : Chessboard.Move.t list
    ; answer : Chessboard.Move.t
    ; srs : Srs.t
    }
  [@@deriving equal, sexp]

  (** [prompt @ [answer]] — the full line from the start through the card. *)
  val line : t -> Chessboard.Move.t list
end

type t =
  { name : string
  ; trainee : Color.t
  ; children : Node.t list (** moves prepared from the initial position *)
  }
[@@deriving equal, sexp]

val create : name:string -> trainee:Color.t -> t

(** Insert a line (moves from the start) into the tree, merging with any
    existing nodes that share a prefix. Newly created nodes start with a fresh
    {!Srs.create}; existing nodes keep their schedule. *)
val add_line : t -> Chessboard.Move.t list -> t

(** Every trainee card in the tree, in depth-first order. *)
val cards : t -> Card.t list

val card_count : t -> int

(** Cards whose schedule says they are due at [now] (epoch ms). *)
val due_cards : t -> now:float -> Card.t list

(** Update the schedule of the node identified by [line] ([prompt @ [answer]]).
    Returns the repertoire unchanged if no such node exists. *)
val update_srs : t -> line:Chessboard.Move.t list -> f:(Srs.t -> Srs.t) -> t
