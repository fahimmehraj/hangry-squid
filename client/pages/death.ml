open! Core
open Bonsai_web

let body (local_ _graph) =
  Bonsai.return
    (Vdom.Node.div [ Vdom.Node.h2 [ Vdom.Node.text "You Died!" ] ])
;;
