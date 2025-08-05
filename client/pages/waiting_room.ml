open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let player_in_waiting_room name ~is_ready =
  let name_color =
    match is_ready with true -> "#32a852" | false -> "#ed5c8a"
  in
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
    width: 200px;
  height: 200px;
  border-radius: 50%; 
  object-fit: cover; 
  display: block; 
  |}]
  in
  Vdom.Node.div
    ~attrs:[ container_styles ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src (Components.url_by_name name); avatar_styles ]
        ()
    ; Vdom.Node.h2
        ~attrs:[ [%css {| color: %{name_color}; |}] ]
        [ Vdom.Node.text name ]
    ]
;;

let ready_button (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let%sub { ready_players; me; _ } = client_state in
  let am_i_ready =
    let%arr ready_players and me in
    List.mem ready_players me.name ~equal:String.equal
  in
  let dispatch_ready =
    Rpc_effect.Rpc.dispatcher Rpcs.Client_message.rpc graph
  in
  let query =
    let%arr me and am_i_ready in
    Rpcs.Client_message.Query.Ready_status_change
      { name = me.name; is_ready = not am_i_ready }
  in
  let%arr am_i_ready and query and dispatch_ready in
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          match%bind.Effect dispatch_ready query with
          | Ok _ -> Effect.all_unit []
          | Error error ->
            Effect.of_sync_fun eprint_s [%sexp (error : Error.t)])
      ]
    [ Vdom.Node.text
        (match am_i_ready with true -> "Unready" | false -> "Ready up")
    ]
;;

let body (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let player_view players ready_players =
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
    display: flex;
    padding: 8px;
    gap: 20px;
    flex-wrap: wrap;
  |}]
        ; Vdom.Attr.class_ "player-container"
        ]
      (List.map players ~f:(fun (player : Restricted_player_view.t) ->
         player_in_waiting_room
           player.name
           ~is_ready:(List.mem ready_players player.name ~equal:String.equal)))
  in
  let ready_button = ready_button client_state graph in
  let%arr { players; ready_players; _ } = client_state
  and ready_button in
  let player_view_component = player_view players ready_players in
  {%html|
    <div class="flex-column items-center">
      <div class="m-4">
        %{ready_button}
      </div>
      %{player_view_component}
    </div>
  |}
;;
