open! Core

(** A portable "home cook": the whole collection of repertoires a user has,
    plus a format version, serialized to a single self-contained file that can
    be downloaded and re-imported on another device.

    The format is versioned so old saves keep loading as the schema evolves —
    never change the shape without bumping {!current_version} and migrating. *)

type t =
  { version : int
  ; repertoires : Repertoire.t list
  }
[@@deriving equal, sexp]

val current_version : int

(** Wrap repertoires in a home cook stamped with the current version. *)
val create : Repertoire.t list -> t

(** Serialize to the on-disk text form (a human-readable sexp). *)
val to_string : t -> string

(** Parse a home cook file, tolerating (migrating) older format versions. *)
val of_string : string -> t Or_error.t
