open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let avatar_url =
  "https://upload.wikimedia.org/wikipedia/en/c/c1/Seong_Gi-hun_season_1.png"
;;

type action =
  { target : Restricted_player_view.t option
  ; items : Item.t list
  }

let player_component player state inject =
  let color =
    match state.target with
    | None -> "transparent"
    | Some target ->
      (match Restricted_player_view.equal target player with
       | true -> "#4BB4FF"
       | false -> "transparent")
  in
  let container_styles =
    [%css
      {|
      border: 1px solid %{color};
      display: flex;
      flex-direction: column;
      align-items: center;
    |}]
  in
  let avatar_styles =
    [%css
      {|
      width: 100px;
      border: 1px solid %{color};
    height: 100px;
    border-radius: 50%; 
    object-fit: cover; 
    display: block; 
    |}]
  in
  Vdom.Node.div
    ~attrs:
      [ container_styles
      ; Vdom.Attr.on_click (fun _ -> inject (`Update_target player))
      ]
    [ Vdom.Node.img ~attrs:[ Vdom.Attr.src avatar_url; avatar_styles ] ()
    ; Vdom.Node.h2
        ~attrs:[ [%css {|
      color: %{color};
    |}] ]
        [ Vdom.Node.text player.name ]
    ]
;;

let player_list
  (players : Restricted_player_view.t list Bonsai.t)
  state
  inject
  =
  let%arr players and state and inject in
  List.map players ~f:(fun player -> player_component player state inject)
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

let left_section players state inject (local_ graph) =
  let%arr player_list_stateful = player_list players state inject in
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
    ; player_list_stateful
    ]
;;

let inventory (items : Item.t list Bonsai.t) state inject =
  let item_components =
    let%arr items and state and inject in
    List.map items ~f:(fun item ->
      let item_selected = List.mem state.items item ~equal:Item.equal in
      Components.item item ~selected:item_selected ~on_click:(fun _ ->
        inject (`Toggle_item item)))
  in
  let%arr item_components in
  Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "inventory" ] ] item_components
;;

let right_section (me : Player.t Bonsai.t) state inject (local_ graph) =
  let items =
    let%arr me in
    me.inventory
  in
  let%arr inventory_stateful = inventory items state inject in
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
  let state, inject =
    Bonsai.state_machine_with_input
      ~default_model:{ target = None; items = [] }
      ~apply_action:(fun ctx input model action ->
        match action with
        | `Update_target target ->
          { target = Some target; items = model.items }
        | `Toggle_item item ->
          (match List.mem model.items item ~equal:Item.equal with
           | false -> { target = model.target; items = item :: model.items }
           | true ->
             { target = model.target
             ; items =
                 List.filter model.items ~f:(fun cur_items ->
                   not (Item.equal cur_items item))
             }))
      client_state
      graph
  in
  let%sub { me; players; _ } = client_state in
  let left_section_stateful = left_section players state inject graph in
  let right_section_stateful = right_section me state inject graph in
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
       [ content_stateful ])
;;
