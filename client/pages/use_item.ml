open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let header =
  {%html|
  <div class="header">
    <h1>Hangry Games</h1>
    <p>Action Phase</p>
    <p>15 seconds left in the current phase</p>
  </div>
|}
;;

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
    ; Vdom.Node.h2 [ Vdom.Node.text name ]
    ]
;;

let players =
  List.init 8 ~f:(fun i -> Printf.sprintf "Player %d" (i + 1))
  |> List.map ~f:(fun player ->
    player_in_waiting_room
      player
      "https://upload.wikimedia.org/wikipedia/en/c/c1/Seong_Gi-hun_season_1.png")
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
    display: flex;
    padding: 20px;
    flex-wrap: wrap;
    gap: 10px;
    border: 2px solid black;
    row-gap: 4px;
    overflow-y: auto;
    min-width: 400px;
    flex-grow: 1;
    |}]
         ]
;;

let left_section =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Select A Player" ]
    ; players
    ]
;;

let inventory =
  [ Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ; Item.pocket_knife
  ]
  |> List.map ~f:Components.item
  |> Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "inventory" ] ]
;;

let right_section =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Your Inventory" ]; inventory ]
;;

let content =
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
    [ left_section; right_section ]
;;

let body (client_state : Client_state.t Bonsai.t) (local_ graph) =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:
         [ [%css
             {|
  height: 100%;
  display: flex;
  flex-direction: column;
|}]
         ]
       [ header; content ])
;;
