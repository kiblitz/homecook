open! Core
open! Import

(** Pure building blocks for the practice view. State lives in the app's state
    machine; these helpers turn it into a board and a grade. *)

(** Replay a line of moves from the start position. *)
val position_after : Homecook_lib.Chessboard.Move.t list -> Homecook_lib.Chessboard.t

(** Grade a card from how it was answered (tries, and whether it was revealed). *)
val grade_of_tries : revealed:bool -> tries:int -> Homecook_lib.Srs.Grade.t

(** A tap-to-move board: renders [position], highlights the selected source
    square and an optional answer [hint] (source, target), and reports each
    tapped square to [on_click]. *)
val render_board
  :  position:Homecook_lib.Chessboard.t
  -> selected:Square.t option
  -> hint:(Square.t * Square.t) option
  -> on_click:(Square.t -> unit Effect.t)
  -> Vdom.Node.t
