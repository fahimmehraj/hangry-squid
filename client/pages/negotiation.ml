open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var
(* open Bonsai.Let_syntax *)

type party =
  | Me
  | Other

let inventory (player: Player.t)=
  List.map player.inventory ~f:Components.item
  |> Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "inventory" ] ]
;;

let msg_bubble content ~who =
  let bubble_color = match who with Me -> "#000000" | Other -> "#E1E1E1" in
  let text_color = match who with Me -> "#ffffff" | Other -> "#000000" in
  let bubble_alignment =
    match who with Me -> "flex-end" | Other -> "flex-start"
  in
  let bubble_styles =
    [%css
      {|
    background-color: %{bubble_color};
    padding: 8px 24px;
    color: %{text_color}; 
    flex-direction: column;
    align-self: %{bubble_alignment};
    align-items: center;
    border-radius: 12px;
    font-family: "Inter";
  |}]
  in
  Vdom.Node.div
    ~attrs:[ bubble_styles ]
    [ Vdom.Node.p [ Vdom.Node.text content ] ]
;;

let players_list_element (players : Restricted_player_view.t list) =
  List.filter players ~f:(fun { is_alive; _ } -> is_alive)
  |> List.map ~f:(fun player ->
    Components.healthbar player.name player.health)
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
      border-right: 1px solid #000000;
      display: flex;
      flex-direction: column;
      gap: 16px;
      overflow-y: auto;
      overflow-x: hidden;
    |}]
         ]
;;

let messages =
  [ msg_bubble "Yo" ~who:Other
  ; msg_bubble "I have a big knife" ~who:Other
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ]
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
    display: flex;
    flex-direction: column;
    flex-grow: 1;
    gap: 2px;
    overflow: auto;
  |}]
         ]
;;

let reply_box =
  Vdom.Node.input
    ~attrs:
      [ [%css
          {|
    position: sticky;
    bottom: 0;
    width: 75%;
    box-sizing: border-box;
    margin-left: 4px;
    padding: 3px 3px;
    font-size: 16px;
  |}]
      ; Vdom.Attr.placeholder "Reply"
      ]
    ()
;;

let messages_window =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    flex-grow: 1;
    overflow: auto;

  |}]
      ]
    [ messages; reply_box ]
;;

let chat_window players my_messages public_messages =
  let%arr players in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    border: 2px solid #000000;
    gap: 8px;
    overflow-y: auto;

  |}]
      ]
    [ players_list_element players ; messages_window ]
;;

let chat_window_with_header players my_messages public_messages =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    overflow-y: auto;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Chat" ]; chat_window players my_messages public_messages ]
;;

let inventory_window_with_header (player : Player.t Bonsai.t) =
  let%arr player in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    overflow-y: auto;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Inventory" ]; inventory player ]
;;

let content (current_state: Client_state.t Bonsai.t) =
  let%sub { me ; players ; my_messages ; public_messages } = current_state in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: grid;
    grid-template-columns: 2fr 1fr;
    gap: 1rem;
    flex-grow: 1;
    overflow-y: auto;
    height: 100%;
    margin: 16px;
  |}]
      ]
    [ chat_window_with_header players my_messages public_messages; inventory_window_with_header me ]
;;

let body (current_state : Client_state.t Bonsai.t) (local_ _graph) =
  let%sub { me; _ } = current_state in
  let%arr me in
  Vdom.Node.div
    ~attrs:
      [ [%css {| height: 100%; display: flex; flex-direction: column; |}] ]
    [ Components.header me ~phase_name:"Negotiation Phase" ~seconds_left:60
    ; content current_state
    ]
;;
