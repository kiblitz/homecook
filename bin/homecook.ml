open! Core
open! Import
module Reorderable_list = Bonsai_web_ui_reorderable_list
module Form = Bonsai_web_ui_form.With_automatic_view

module Stylesheet =
  [%css
    stylesheet
      {|
    html, body {
      background-color: #322f2c !important;
      margin: 0;
      height: 100vh;
      width: 100vw;
    }
  |}]

let () = Bonsai_web.Start.start Chessboard.component
