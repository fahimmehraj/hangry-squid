open! Core
open Bonsai_web

let body () =
  Bonsai.return
    (Vdom.Node.div 
    ~attrs:[ [%css {|
       height: 100%;
       display: flex;
       justify-content: center;
       align-items: center;
    |}] ]
    [ Vdom.Node.h2 [ Vdom.Node.text "You Died!" ] ])
;;
