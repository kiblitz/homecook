open! Core
open! Import

val component : ?width:int -> ?height:int -> Bonsai.graph -> Vdom.Node.t Bonsai.t
