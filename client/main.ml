open! Core
open! Async_kernel
open! Async_rpc_kernel

(* open Async_js *)
open Bonsai_web

(* open Async_kernel.Let_syntax *)
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let loading_page = {%html|
  <p>Loading...</p>
|}

let page_with_header (current_state : Client_state.t Bonsai.t) (local_ graph)
  =
  let%sub { round_start; current_phase; me; _ } = current_state in
  let now =
    Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph
  in
  let header =
    let%arr now and round_start and me and current_phase in
    let seconds_left =
      Time_ns.Span.(
        (Game_phase.to_duration current_phase |> Time_ns.Span.of_int_sec)
        - Time_ns.abs_diff now round_start
        |> Time_ns.Span.to_int_sec)
    in
    Components.header me ~phase:current_phase ~seconds_left
  in
  let content =
    match%sub current_phase with
    | Waiting_room -> Pages.Waiting_room.body current_state graph
    | Rules -> Pages.Rules.body ()
    | Item_selection -> Pages.Select.body current_state graph
    | Negotiation -> Pages.Negotiation.body current_state graph
    | Item_usage -> Pages.Use_item.body current_state graph
    | Round_results -> Pages.Outcome.body current_state graph
    | Game_results -> Pages.Game_over.body current_state graph
  in
  let%arr header and content in
  Vdom.Node.div [ header; content ]
;;

let render_game_page name (local_ graph) =
  let client_state : Client_state.t option Bonsai.t =
    let query =
      let%arr name in
      { Rpcs.Poll_client_state.Query.name }
    in
    let result =
      Rpc_effect.Polling_state_rpc.poll
        Rpcs.Poll_client_state.rpc
        ~equal_query:[%equal: Rpcs.Poll_client_state.Query.t]
        ~every:(Bonsai.return (Time_ns.Span.of_sec 0.1))
        ~where_to_connect:
          (Bonsai.return
             (Rpc_effect.Where_to_connect.self
                ~on_conn_failure:
                  Rpc_effect.On_conn_failure.Surface_error_to_rpc
                ()))
        query
        graph
    in
    let%arr result in
    match result.last_ok_response with
    | None -> None
    | Some (_query, resp) -> Some resp
  in
  match%sub client_state with
  | None -> Bonsai.return loading_page
  | Some client_state -> page_with_header client_state graph
;;

let render_landing_page update_join_game_state error =
  let%arr update_join_game_state and error in
  let join_game_button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            update_join_game_state `Try_to_join_game)
        ]
      [ Vdom.Node.text "Join Game" ]
  in
  let select_name_input =
    Vdom.Node.input
      ~attrs:
        [ Vdom.Attr.placeholder "Enter name"
        ; Vdom.Attr.type_ "text"
        ; Vdom.Attr.on_input (fun _ current_name ->
            update_join_game_state (`Update_name current_name))
        ]
      ()
  in
  match error with
  | None -> Vdom.Node.div [ select_name_input; join_game_button ]
  | Some error_message ->
    Vdom.Node.div
      [ select_name_input; join_game_button; Vdom.Node.text error_message ]
;;

let serve_route (local_ graph) =
  let dispatcher =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Client_message.rpc
      graph
      ~where_to_connect:
        (Bonsai.return
           (Rpc_effect.Where_to_connect.self
              ~on_conn_failure:
                Rpc_effect.On_conn_failure.Surface_error_to_rpc
              ()))
  in
  let join_game_state, update_join_game_state =
    Bonsai.state_machine_with_input
      ~default_model:(Entering_name ("", None) : Landing_state.t)
      ~apply_action:(fun ctx input (model : Landing_state.t) action ->
        match input with
        | Active dispatcher ->
          (match action with
           | `Ack_join accepted_name -> Accepted_name accepted_name
           | `Failed_join error ->
             Entering_name
               (Landing_state.name model, Some (Error.to_string_hum error))
           | `Update_name new_name -> Entering_name (new_name, None)
           | `Try_to_join_game ->
             let player_name_query =
               Rpcs.Client_message.Query.New_player
                 (Landing_state.name model)
             in
             let my_new_effect =
               let%bind.Effect result = dispatcher player_name_query in
               match result with
               | Error error ->
                 Bonsai.Apply_action_context.inject ctx (`Failed_join error)
               | Ok _ ->
                 Bonsai.Apply_action_context.inject
                   ctx
                   (`Ack_join (Landing_state.name model))
             in
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               my_new_effect;
             model)
        | Inactive -> model)
      dispatcher
      graph
  in
  match%sub join_game_state with
  | Accepted_name name -> render_game_page name graph
  | Entering_name (name, error) ->
    render_landing_page update_join_game_state error
;;

let () = Bonsai_web.Start.start serve_route
