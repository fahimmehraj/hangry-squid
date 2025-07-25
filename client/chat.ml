open! Core
open Bonsai_web
(* open Bonsai.Let_syntax *)

(* An example of a small Bonsai component for creating counter
   widgets ("+" and "-" buttons allow you to increment and decrement a
   number). The state of each counter is housed in a shared [Map]. Adding
   a new counter is as simple as adding a value to the map. *)

let header =
  {%html|
  <div>
    <h1>Hangry Games</h1>
    <p>60 seconds left</p>
  </div>
|}
;;

let body () = Bonsai.return (Vdom.Node.div [ header ])