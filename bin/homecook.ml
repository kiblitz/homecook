open! Core
open! Import
module Reorderable_list = Bonsai_web_ui_reorderable_list
module Form = Bonsai_web_ui_form.With_automatic_view

module Stylesheet =
  [%css
    stylesheet
      {|
  |}]

let () = Bonsai_web.Start.start Chessboard.component
