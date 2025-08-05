open! Core
open Bonsai_web
open Hangry_squid
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var

let player_in_waiting_room name avatar_url =
  let container_styles =
    [%css
      {|
    display: flex;
    flex-direction: column;
    align-items: center;
  |}]
  in
  let avatar_styles =
    [%css
      {|
    width: 100px;
  height: 100px;
  border-radius: 50%; 
  object-fit: cover; 
  display: block; 
  |}]
  in
  Vdom.Node.div
    ~attrs:[ container_styles ]
    [ Vdom.Node.img ~attrs:[ Vdom.Attr.src avatar_url; avatar_styles ] ()
    ; Vdom.Node.p [ Vdom.Node.text name ]
    ]
;;

let result_component (result : Round_result.t) =
  let player_name = result.player_in_question in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: row;
    align-items: center;
    gap: 24px;
  |}]
      ]
    [ player_in_waiting_room player_name (Components.url_by_name player_name)
    ; Vdom.Node.p [ Vdom.Node.text result.message ]
    ]
;;

let private_results results =
  List.map results ~f:result_component
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
  display: flex;
  flex-direction: column;
  gap: 8px;
|}]
         ]
;;

let public_results results =
  List.map results ~f:result_component
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
  display: flex;
  flex-direction: column;
  gap: 8px;
|}]
         ]
;;

let left_section results =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Private Results" ]
    ; private_results results
    ]
;;

let right_section results =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Public Results" ]
    ; public_results results
    ]
;;

let content (client_state : Client_state.t Bonsai.t) =
  let%sub { my_results; public_results; _ } = client_state in
  let%arr my_results and public_results in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    justify-content: space-around;
    margin: 8px;
    gap: 8px;
    flex-grow: 1;
  |}]
      ]
    [ left_section my_results; right_section public_results ]
;;

let body (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let%arr content = content client_state in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
  height: 100%;
  display: flex;
  flex-direction: column;
|}]
      ]
    [ content ]
;;
