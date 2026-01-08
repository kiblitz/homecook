open! Core

module T = struct
  type t =
    { file : File.t
    ; rank : Rank.t
    }
  [@@deriving compare, equal, sexp_of]
end

module Delta = struct
  type t =
    { file : int
    ; rank : int
    }
end

include T

let to_string { file; rank } = [%string "%{file#File}%{rank#Rank}"]

let validate ?file_upper_bound ?rank_upper_bound { file; rank } =
  let%map.Or_error file = File.validate ?upper_bound:file_upper_bound file
  and rank = Rank.validate ?upper_bound:rank_upper_bound rank in
  { file; rank }
;;

let ( + ) { file; rank } { Delta.file = d_file; rank = d_rank } =
  let%map.Or_error file = File.(file + d_file)
  and rank = Rank.(rank + d_rank) in
  { file; rank }
;;

include Comparable.Make_plain (T)
