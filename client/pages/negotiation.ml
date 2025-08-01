open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var
(* open Bonsai.Let_syntax *)

type party =
  | Me
  | Other

type tab =
  | All
  | Dm of string

let inventory (player : Player.t) =
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

let all_messages_tab_element (tab : tab) (set_tab : tab -> unit Ui_effect.t) =
  let background_color =
    match tab with All -> "#F7F7F7" | _ -> "transparent"
  in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.on_click (fun _ -> set_tab All)
      ; [%css
          {|
      background-color: %{background_color}; 
      cursor: pointer;
    |}]
      ]
    [ Vdom.Node.p [ Vdom.Node.text "All Messages" ] ]
;;

let players_list_element
  (players : Restricted_player_view.t list)
  (tab : tab)
  (set_tab : tab -> unit Ui_effect.t)
  ~(me : Player.t)
  =
  let players_list =
    List.filter players ~f:(fun { is_alive; _ } -> is_alive)
    |> List.filter ~f:(fun player -> not (String.equal player.name me.name))
    |> List.map ~f:(fun player ->
      let healthbar = Components.healthbar player.name player.health in
      let background_color =
        match tab with
        | All -> "transparent"
        | Dm name ->
          (match String.equal name player.name with
           | true -> "#F7F7F7"
           | false -> "transparent")
      in
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
    background-color: %{background_color}; 
    cursor: pointer;
  |}]
          ; Vdom.Attr.on_click (fun _ -> set_tab (Dm player.name))
          ]
        [ healthbar ])
  in
  Vdom.Node.div
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
    (all_messages_tab_element tab set_tab :: players_list)
;;

let message_bubbles (messages : Message.t list) (me : string) =
  List.map messages ~f:(fun message ->
    let who =
      match String.equal message.sender me with true -> Me | false -> Other
    in
    msg_bubble message.contents ~who)
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

let messages_window messages ~me =
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
    [ message_bubbles messages me; reply_box ]
;;

let chat_window
  players
  my_messages
  public_messages
  ~(me : Player.t Bonsai.t)
  (local_ graph)
  =
  let tab, set_tab = Bonsai.state All graph in
  let%arr players
  and tab
  and set_tab
  and my_messages
  and public_messages
  and me in
  let messages =
    match tab with
    | All -> public_messages
    | Dm other_party ->
      (match Map.find my_messages other_party with
       | None -> []
       | Some messages -> messages)
  in
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
    [ players_list_element players tab set_tab ~me
    ; messages_window messages ~me:me.name
    ]
;;

let chat_window_with_header
  players
  my_messages
  public_messages
  ~me
  (local_ graph)
  =
  let chat_window_stateful =
    chat_window players my_messages public_messages ~me graph
  in
  let%arr chat_window_stateful in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    overflow-y: auto;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Chat" ]; chat_window_stateful ]
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

let content (current_state : Client_state.t Bonsai.t) (local_ graph) =
  let%sub { me; players; my_messages; public_messages; _ } = current_state in
  let chat_section_stateful =
    chat_window_with_header players my_messages public_messages ~me graph
  in
  let inventory_section_stateful = inventory_window_with_header me in
  let%arr chat_section_stateful and inventory_section_stateful in
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
    [ chat_section_stateful; inventory_section_stateful ]
;;

let body (current_state : Client_state.t Bonsai.t) (local_ graph) =
  let%sub { me; _ } = current_state in
  let content = content current_state graph in
  let%arr me and content in
  Vdom.Node.div
    ~attrs:
      [ [%css {| height: 100%; display: flex; flex-direction: column; |}] ]
    [ content ]
;;
