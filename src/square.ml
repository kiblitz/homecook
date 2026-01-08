open! Core

module T = struct
  type t =
    { file : File.t
    ; rank : Rank.t
    }
  [@@deriving compare, equal, sexp_of]

  let to_string { file; rank } = [%string "%{file#File}%{rank#Rank}"]
end

include T
include Comparable.Make_plain (T)
