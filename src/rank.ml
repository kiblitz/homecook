open! Core

module T = struct
  type t = int [@@deriving compare, sexp_of]
end

include T

let validate ?upper_bound t =
  if Int.is_negative t
  then Or_error.error_s [%message "Cannot have a negative rank" (t : t)]
  else (
    match upper_bound with
    | None -> Ok t
    | Some upper_bound when t < upper_bound -> Ok t
    | Some upper_bound ->
      Or_error.error_s [%message "Rank exceeds upper bound" (t : t) (upper_bound : int)])
;;

let of_idx i = validate i
let to_idx t = validate t
let of_int i = validate (i - 1)

let to_int i =
  let%map.Or_error i = validate i in
  i + 1
;;

let to_string t = Int.to_string (to_int t |> ok_exn)

let ( + ) t d =
  let%bind.Or_error i = to_idx t in
  of_idx (i + d)
;;

include Comparable.Make_plain (T)
