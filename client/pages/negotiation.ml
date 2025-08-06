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
  | Dm of Restricted_player_view.t

type read_status =
  { messages_read : int
  ; messages_unread : int
  }

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

let all_messages_tab_element
  (tab : tab Bonsai.t)
  (set_tab : (tab -> unit Ui_effect.t) Bonsai.t)
  (msg_count : int Bonsai.t)
  (local_ graph)
  =
  let unread_messages, inject = Bonsai.state 0 graph in
  Bonsai.Edge.on_change'
    ~equal:Int.equal
    ~callback:
      (let%arr inject and tab and unread_messages in
       fun prev cur ->
         let prev = match prev with None -> 0 | Some prev -> prev in
         match cur = prev with
         | true -> inject 0
         | false ->
           print_s [%sexp (cur : int)];
           (match tab with
            | All -> inject 0
            | Dm _ -> inject (unread_messages + (cur - prev))))
    msg_count
    graph;
  let unread_badge =
    let%arr unread_messages in
    match unread_messages with
    | 0 -> Vdom.Node.none
    | count ->
      Vdom.Node.div
        ~attrs:
          [ [%css
              {|
    background-color: #ff4500;
    color: white;
    font-size: 12px;
    font-weight: bold;
    border-radius: 50%;
    width: 20px; 
    height: 20px;
    display: flex;
    align-items: center;
    justify-content: center; 
  |}]
          ]
        [ Vdom.Node.text (Int.to_string count) ]
  in
  let background_color =
    let%arr tab in
    match tab with All -> "#F7F7F7" | _ -> "transparent"
  in
  let%arr set_tab and background_color and unread_badge in
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.on_click (fun _ -> set_tab All)
      ; [%css
          {|
      background-color: %{background_color}; 
      cursor: pointer;
      display: flex;
      gap: 8px;
    font-family: "Inter";
      height: 40px;
    |}]
      ]
    [ Vdom.Node.div [ Vdom.Node.p [ Vdom.Node.text "All Messages" ] ]
    ; unread_badge
    ]
;;

let player_tab_element
  (player : Restricted_player_view.t Bonsai.t)
  (tab : tab Bonsai.t)
  (set_tab : (tab -> unit Ui_effect.t) Bonsai.t)
  (msg_count : int Bonsai.t)
  (local_ graph)
  =
  (* let read_status, inject =
    Bonsai.state_machine
      ~default_model:{ messages_read = 0; messages_unread = 0 }
      ~apply_action:(fun ctx model action ->
        match action with
        | `Read_all ->
          { messages_read = model.messages_read + model.messages_unread ; messages_unread = 0 }
        | `New_message num_messages ->
          if num_messages = model.messages_read
          then model
          else { model with messages_unread = num_messages - model.messages_read })
      graph
  in *)
  let unread_count, inject = Bonsai.state 0 graph in
  Bonsai.Edge.on_change'
    ~equal:Int.equal
    ~callback:
      (let%arr inject and tab and player and unread_count in
       fun prev cur ->
         let prev = match prev with None -> 0 | Some prev -> prev in
         match cur = prev with
         | true -> inject 0
         | false ->
           print_s [%sexp (cur : int)];
           (match tab with
            | All -> inject (unread_count + (cur - prev))
            | Dm selected_player ->
              if Restricted_player_view.equal selected_player player
              then inject 0
              else inject (unread_count + (cur - prev))))
    msg_count
    graph;
  let%arr player and tab and set_tab and inject and unread_count in
  let element =
    let healthbar =
      Components.healthbar ~unread_count player.name player.health
    in
    let background_color =
      match tab with
      | All -> "transparent"
      | Dm plr ->
        (match String.equal plr.name player.name with
         | true -> "#F7F7F7"
         | false -> "transparent")
    in
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
    display: flex;
    justify-content: space-between;
    align-items: center;
    background-color: %{background_color}; 
    cursor: pointer;
  |}]
        ; Vdom.Attr.on_click (fun _ ->
            let%bind.Effect () = set_tab (Dm player) in
            let%bind.Effect () = inject 0 in
            Effect.return ())
        ]
      [ healthbar ]
  in
  element
;;

let sidebar
  (players_list : Vdom.Node.t list)
  (all_message_element : Vdom.Node.t)
  =
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
    (all_message_element :: players_list)
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
                    ; recipient = Some recipient.name
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
      margin-right: 6px;
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

let auto_scrolling_div =
  Vdom.Node.div ~attrs:[ Vdom.Attr.id "scrollable-div" ] []
;;

let messages_window
  messages
  ~(me : Player.t Bonsai_web.Bonsai.t)
  tab
  (local_ graph)
  =
  let reply_and_send_container = reply_and_send_container tab me graph in
  let d = auto_scrolling_div in
  let length =
    let%arr messages in
    List.length messages
  in
  Bonsai.Edge.on_change
    ~equal:Int.equal
    length
    ~callback:
      (Bonsai.return (fun a ->
         let _ =
           Js_of_ocaml.Js.Unsafe.eval_string
             "\n\
             \  const element = document.getElementById(\"scrollable-div\");\n\
             \  try {if (element !== null) {\n\
             \    element.scrollTo(0, element.scrollHeight);\n\
             \  }}\n\
             \    catch (err) {\n\
             \      ;\n\
             \    }\n\
             \         "
         in
         Effect.return ()))
    graph;
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
    [ message_bubbles messages me.name; d; reply_and_send_container ]
;;

let chat_window
  players
  my_messages
  public_messages
  ~(me : Player.t Bonsai.t)
  (local_ graph)
  =
  let players =
    let%arr players and me in
    List.filter players ~f:(fun { Restricted_player_view.is_alive; _ } ->
      is_alive)
    |> List.filter ~f:(fun player -> not (String.equal player.name me.name))
  in
  let my_messages =
    let%arr players and my_messages in
    List.map players ~f:(fun player ->
      ( player
      , match Map.find my_messages player.name with
        | None -> []
        | Some messages -> messages ))
    |> Map.of_alist_exn (module Restricted_player_view)
  in
  let tab, set_tab = Bonsai.state All graph in
  let player_tabs =
    Bonsai.assoc
      (module Restricted_player_view)
      my_messages
      ~f:(fun player messages graph ->
        let msg_count =
          let%arr messages in
          List.length messages
        in
        player_tab_element player tab set_tab msg_count graph)
      graph
  in
  let public_msg_count =
    let%arr public_messages in
    List.length public_messages
  in
  let all_messages_tab_element =
    all_messages_tab_element tab set_tab public_msg_count graph
  in
  let sidebar =
    let%arr player_tabs and tab and set_tab and all_messages_tab_element in
    sidebar (Map.data player_tabs) all_messages_tab_element
  in
  let messages =
    let%arr players and tab and my_messages and public_messages in
    match tab with
    | All -> public_messages
    | Dm other_party ->
      (match Map.find my_messages other_party with
       | None -> []
       | Some messages -> messages)
  in
  let messages_window_stateful = messages_window messages ~me tab graph in
  let%arr messages_window_stateful and sidebar in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    border: 2px solid #000000;
    gap: 8px;
    overflow-y: scroll;
    height: 65vh;
  |}]
      ]
    [ sidebar; messages_window_stateful ]
;;

(* Remove unnecessary _with_header *)
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
  let content = content current_state graph in
  let%arr content in
  Vdom.Node.div
    ~attrs:
      [ [%css {| height: 100%; display: flex; flex-direction: column; |}] ]
    [ content ]
;;
