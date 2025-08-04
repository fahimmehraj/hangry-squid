open! Core
open Hangry_squid
open Bonsai.Let_syntax
open Bonsai_web

let body (client_state : Client_state.t Bonsai.t) (local_ _graph) =
  let%sub { players } = client_state in
  let amount_won =
    let%arr players in
    100000 * List.length players
  in
  let%arr amount_won in
  Bonsai.return
    (Vdom.Node.div
       ~attrs:
         [ [%css
             {|
       height: 100%;
       display: flex;
       justify-content: center;
       align-items: center;
    |}]
         ]
       [ Vdom.Node.h2
           [ Vdom.Node.text ("You won $" ^ Int.to_string amount_won ^ "!") ]
       ])
;;
