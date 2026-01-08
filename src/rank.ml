open! Core

module T = struct
  type t = int [@@deriving compare, sexp_of]

  let of_idx_exn = Fn.id
  let to_idx = Fn.id
  let of_int_exn i = i - 1
  let to_int i = i + 1
  let to_string t = Int.to_string (to_int t)
end

include T
include Comparable.Make_plain (T)
