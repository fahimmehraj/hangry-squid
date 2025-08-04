open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let items_to_select_from
  (item_choices : (Item.t * Item.t) option Bonsai.t)
  ~(me : Player.t Bonsai.t)
  (local_ graph)
  =
  let selected, toggle_selected = Bonsai.state false graph in

  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr toggle_selected in
       toggle_selected false)
    graph;


  let dispatch_item_select =
    Rpc_effect.Rpc.dispatcher Rpcs.Client_message.rpc graph
  in
  let%arr item_choices
  and dispatch_item_select
  and me
  and toggle_selected
  and selected in
  let query item =
    Rpcs.Client_message.Query.Item_selection { name = me.name; item }
  in
  let on_click item _ =
    match%bind.Effect dispatch_item_select (query item) with
    | Ok _ -> toggle_selected true
    | Error error -> Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]
  in
  match item_choices, selected with
  | None, _ -> Vdom.Node.text "You have been blocked from using an item this round"
  | _, true -> Vdom.Node.text "You have selected an item"
  | Some (item1, item2), false ->
    Vdom.Node.div
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
      [ Components.item item1 ~on_click:(on_click item1)
      ; Components.item item2 ~on_click:(on_click item2)
      ]
;;

let left_section
  (item_choices : (Item.t * Item.t) option Bonsai.t)
  ~(me : Player.t Bonsai.t)
  (local_ graph)
  =
  let items_to_select_from = items_to_select_from item_choices ~me graph in
  let%arr items_to_select_from in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Select Your Item" ]
    ; items_to_select_from
    ]
;;

let inventory (player : Player.t Bonsai.t) =
  let%arr player in
  List.map player.inventory ~f:Components.item
  |> Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "inventory" ] ]
;;

let right_section (player : Player.t Bonsai.t) =
  let inventory_stateful = inventory player in
  let%arr inventory_stateful in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Your Inventory" ]; inventory_stateful ]
;;

let content (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let%sub { me; item_choices; _ } = client_state in
  let left_section_stateful = left_section item_choices ~me graph in
  let right_section_stateful = right_section me in
  let%arr left_section_stateful and right_section_stateful in
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
    [ left_section_stateful; right_section_stateful ]
;;

let body (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let%arr content_stateful = content client_state graph in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
  height: 100%;
  display: flex;
  flex-direction: column;
|}]
      ]
    [ content_stateful ]
;;
