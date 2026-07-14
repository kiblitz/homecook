open! Core
open! Import
module Home_cook = Homecook_lib.Home_cook
module Dom_html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Url = Js_of_ocaml.Url

let key = "homecook.v1"

let load () =
  Js.Optdef.case
    Dom_html.window##.localStorage
    (fun () -> None)
    (fun storage ->
      Js.Opt.case
        (storage##getItem (Js.string key))
        (fun () -> None)
        (fun value -> Home_cook.of_string (Js.to_string value) |> Result.ok))
;;

let save home_cook =
  Effect.of_sync_fun
    (fun () ->
      Js.Optdef.iter Dom_html.window##.localStorage (fun storage ->
        storage##setItem (Js.string key) (Js.string (Home_cook.to_string home_cook))))
    ()
;;

let download home_cook =
  Effect.of_sync_fun
    (fun () ->
      let href =
        "data:application/octet-stream;charset=utf-8,"
        ^ Url.urlencode (Home_cook.to_string home_cook)
      in
      let a = Dom_html.createA Dom_html.document in
      a##.href := Js.string href;
      a##setAttribute (Js.string "download") (Js.string "cook.homecook");
      Dom.appendChild Dom_html.document##.body a;
      (Js.Unsafe.meth_call a "click" [||] : unit);
      Dom.removeChild Dom_html.document##.body a)
    ()
;;
