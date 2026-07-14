open! Core

type t =
  { version : int
  ; repertoires : Repertoire.t list
  }
[@@deriving equal, sexp]

let current_version = 1
let create repertoires = { version = current_version; repertoires }
let to_string t = Sexp.to_string_hum (sexp_of_t t)

let migrate t =
  (* Only one version exists so far; future migrations chain here. *)
  if t.version > current_version
  then
    Or_error.error_s
      [%message
        "home cook was saved by a newer version of homecook"
          ~file_version:(t.version : int)
          ~supported:(current_version : int)]
  else Ok { t with version = current_version }
;;

let of_string s =
  let%bind.Or_error parsed =
    Or_error.try_with (fun () -> t_of_sexp (Sexp.of_string s))
  in
  migrate parsed
;;

let%test_module _ =
  (module struct
    let sq s =
      let file = File.of_char (Char.uppercase s.[0]) |> ok_exn in
      let rank = Rank.of_int (Char.to_int s.[1] - Char.to_int '0') |> ok_exn in
      { Square.file; rank }
    ;;

    let mv a b = { Chessboard.Move.source = sq a; target = sq b }

    let sample =
      Repertoire.create ~name:"Italian" ~trainee:White
      |> Fn.flip
           Repertoire.add_line
           [ mv "E2" "E4"; mv "E7" "E5"; mv "G1" "F3"; mv "B8" "C6"; mv "F1" "C4" ]
      |> List.return
      |> create
    ;;

    let%expect_test "round-trips through the file format" =
      let reloaded = of_string (to_string sample) |> ok_exn in
      printf "%b\n" (equal sample reloaded);
      [%expect {| true |}]
    ;;

    let%expect_test "rejects a save from a newer format version" =
      let bumped = to_string { sample with version = current_version + 1 } in
      printf "%b\n" (Or_error.is_error (of_string bumped));
      [%expect {| true |}]
    ;;
  end)
;;
