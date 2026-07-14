open! Core
open! Import

(** Top-level home-cook app shell: navigation between the repertoire list and
    the free-play board, and (eventually) the editor and practice modes. *)
val component : Bonsai.graph -> Vdom.Node.t Bonsai.t
