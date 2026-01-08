open! Core

module T = struct
  type t = int [@@deriving compare, sexp_of]
end

include T

let is_valid_char c = Char.('A' <= c && c <= 'Z')

let to_char t =
  match Char.of_int (t + Char.to_int 'A') with
  | Some c when is_valid_char c -> Ok c
  | Some c -> Or_error.error_s [%message "max 26 files" (c : char)]
  | None -> Or_error.error_s [%message "char encoding out of range" (t : int)]
;;

let of_char c =
  if is_valid_char c
  then Ok (Char.to_int c - Char.to_int 'A')
  else Or_error.error_s [%message "Invalid char, expected capital alpha" (c : char)]
;;

let validate ?upper_bound t =
  let%bind.Or_error () = to_char t |> Or_error.ignore_m in
  match upper_bound with
  | None -> Ok t
  | Some upper_bound ->
    let%bind.Or_error upper_bound_t = of_char upper_bound in
    if t > upper_bound_t
    then
      Or_error.error_s [%message "File exceeds upper bound" (t : t) (upper_bound : char)]
    else Ok t
;;

let to_idx t = validate t
let of_idx i = validate i
let to_string t = to_char t |> ok_exn |> Char.to_string

let ( + ) t d =
  let%bind.Or_error i = to_idx t in
  of_idx (i + d)
;;

include Comparable.Make_plain (T)
