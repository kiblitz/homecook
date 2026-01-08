open! Core

module T = struct
  type t = int [@@deriving compare, sexp_of]

  let is_valid_char c = Char.('A' <= c && c <= 'Z')
  let to_idx = Fn.id
  let of_idx_exn = Fn.id

  let to_char t =
    match Char.of_int (t + Char.to_int 'A') with
    | Some c when is_valid_char c -> c
    | Some c -> raise_s [%message "max 26 files" (c : char)]
    | None -> raise_s [%message "char encoding out of range" (t : int)]
  ;;

  let of_char_exn c =
    if is_valid_char c
    then Char.to_int c - Char.to_int 'A'
    else raise_s [%message "Invalid char, expected capital alpha" (c : char)]
  ;;

  let to_string t = to_char t |> Char.to_string
end

include T
include Comparable.Make_plain (T)
