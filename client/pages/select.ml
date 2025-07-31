open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let header =
  {%html|
  <div class="header">
    <h1>Hangry Games</h1>
    <p>Item Selection Phase</p>
    <p>15 seconds left in the current phase</p>
  </div>
|}
;;

let items_to_select_from items =
  List.map items ~f:Components.item
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

let random_items = [ Item.medical_kit; Item.observer ]

let left_section =
  Vdom.Node.div
  ~attrs:[[%css {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]]
    [ Vdom.Node.h2 [ Vdom.Node.text "Select Your Item" ]
    ; items_to_select_from random_items
    ]
;;

let inventory =
  [ Item.pocket_knife; Item.pocket_knife; Item.pocket_knife
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
  ~attrs:[[%css {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]]
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

let body (client_state : Client_state.t Bonsai.t) (local_ graph) = Bonsai.return (Vdom.Node.div 
~attrs:[ [%css {|
  height: 100%;
  display: flex;
  flex-direction: column;
|}]]
[ header ; content ])
