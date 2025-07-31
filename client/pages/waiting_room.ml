open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let avatar_url =
  "https://upload.wikimedia.org/wikipedia/en/c/c1/Seong_Gi-hun_season_1.png"
;;

let header =
  {%html|
  <div class="header">
    <h1>Hangry Games</h1>
    <p>Waiting for games to start</p>
  </div>
|}
;;

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
    [ Vdom.Node.img ~attrs:[ Vdom.Attr.src avatar_url; avatar_styles ] ()
    ; Vdom.Node.h2
        ~attrs:[ [%css {| color: %{name_color}; |}] ]
        [ Vdom.Node.text name ]
    ]
;;

(* let component (local_ graph) =
  let state, inject =
    Bonsai.state_machine
      ~default_model:Int.Map.empty
      ~apply_action:(fun _ctx model -> function
        | `New_counter -> Map.add_exn model ~key:(Map.length model) ~data:0
        | `Incr_by (counter_id, diff) ->
          Map.update model counter_id ~f:(function
            | None -> diff
            | Some x -> x + diff))
      graph
  in
  let%arr state and inject in
  let button text action =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject action) ]
      [ Vdom.Node.text text ]
  in
  let add_button = button "add" `New_counter in
  let for_each (i, c) =
    Vdom.Node.div
      [ button "-1" (`Incr_by (i, -1))
      ; Vdom.Node.textf "%d" c
      ; button "+1" (`Incr_by (i, 1))
      ]
  in
  let counters = state |> Map.to_alist |> List.map ~f:for_each in
  (* let debug =
    Vdom.Node.p [ Vdom.Node.text (Sexp.to_string (Page.sexp_of_t route)) ]
  in *)
  Vdom.Node.div (add_button :: counters)
;; *)

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
        (match am_i_ready with true -> "unready" | false -> "ready up")
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
        ]
      (List.map players ~f:(fun (player : Restricted_player_view.t) ->
         player_in_waiting_room
           player.name
           ~is_ready:(List.mem ready_players player.name ~equal:String.equal)))
  in
  let ready_button = ready_button client_state graph in
  let%arr { players; ready_players; _ } = client_state
  and ready_button in
  Vdom.Node.div [ header; ready_button; player_view players ready_players ]
;;
