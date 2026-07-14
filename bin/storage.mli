open! Core
open! Import

(** Offline persistence for the home cook. State lives entirely in the browser:
    it is mirrored into [localStorage] (survives reloads and restarts, no
    server) and can be exported to / imported from a portable file. *)

(** Read the saved home cook from browser storage. [None] if nothing is stored
    yet or the stored value can't be parsed. *)
val load : unit -> Homecook_lib.Home_cook.t option

(** Persist the home cook to browser storage. *)
val save : Homecook_lib.Home_cook.t -> unit Effect.t

(** Download the home cook as a portable [.homecook] file. *)
val download : Homecook_lib.Home_cook.t -> unit Effect.t
