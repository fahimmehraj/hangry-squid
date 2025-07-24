open! Core
open Bonsai_web
open Bonsai.Let_syntax

(* An example of a small Bonsai component for creating counter
   widgets ("+" and "-" buttons allow you to increment and decrement a
   number). The state of each counter is housed in a shared [Map]. Adding
   a new counter is as simple as adding a value to the map. *)

let header =
  {%html|
  <div>
    <h1>Hangry Games</h1>
    <p>Waiting for games to start</p>
  </div>
|}
;;