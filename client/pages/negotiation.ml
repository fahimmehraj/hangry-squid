open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

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
  List.map (List.rev messages) ~f:(fun message ->
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

let reply_and_send_container
  (tab : tab Bonsai_web.Bonsai.t)
  (me : Player.t Bonsai_web.Bonsai.t)
  (local_ graph)
  =
  let dispatcher = Rpc_effect.Rpc.dispatcher Rpcs.Client_message.rpc graph in
  let im_a_genius =
    let%arr me and tab and dispatcher in
    me, tab, dispatcher
  in
  let state, inject =
    Bonsai.state_machine_with_input
      ~default_model:""
      ~apply_action:(fun ctx input model action ->
        match input with
        | Bonsai.Computation_status.Inactive -> model
        | Active ((me : Player.t), tab, dispatcher) ->
          (match action with
           | `Update_message new_message -> new_message
           | `Send_message ->
             let query =
               Rpcs.Client_message.Query.Chat_message
                 (match tab with
                  | All ->
                    { sender = me.name
                    ; recipient = None
                    ; contents = model
                    ; timestamp = Time_ns.now ()
                    }
                  | Dm recipient ->
                    { sender = me.name
                    ; recipient = Some recipient
                    ; contents = model
                    ; timestamp = Time_ns.now ()
                    })
             in
             if not (String.equal model "")
             then
               Bonsai_web.Bonsai.Apply_action_context.schedule_event
                 ctx
                 (match%bind.Effect dispatcher query with
                  | Ok _ -> Effect.all_unit []
                  | Error error ->
                    Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             ""))
      im_a_genius
      graph
  in
  let%arr state and inject in
  let send_button =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Send_message) ]
      [ Vdom.Node.text "Reply" ]
  in
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
        ; Vdom.Attr.type_ "text"
        ; Vdom.Attr.string_property "value" state
        ; Vdom.Attr.on_keydown (fun event ->
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
            | Enter -> inject `Send_message
            | _ -> Effect.all_unit [])
        ; Vdom.Attr.on_input (fun _ current_message ->
            inject (`Update_message current_message))
        ]
      ()
  in
  Vdom.Node.div [ reply_box; send_button ]
;;

let messages_window
  messages
  ~(me : Player.t Bonsai_web.Bonsai.t)
  tab
  (local_ graph)
  =
  let reply_and_send_container = reply_and_send_container tab me graph in
  let%arr messages and me and reply_and_send_container in
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
    [ message_bubbles messages me.name; reply_and_send_container ]
;;

let chat_window
  players
  my_messages
  public_messages
  ~(me : Player.t Bonsai.t)
  (local_ graph)
  =
  let tab, set_tab = Bonsai.state All graph in
  let messages =
    let%arr players
    and tab
    and set_tab
    and my_messages
    and public_messages
    and me in
    match tab with
    | All -> public_messages
    | Dm other_party ->
      (match Map.find my_messages other_party with
       | None -> []
       | Some messages -> messages)
  in
  let messages_window_stateful = messages_window messages ~me tab graph in
  let%arr messages_window_stateful and players and tab and set_tab and me in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    border: 2px solid #000000;
    gap: 8px;
    overflow-y: scroll;
    height: 76vh;
  |}]
      ]
    [ players_list_element players tab set_tab ~me
    ; messages_window_stateful
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
