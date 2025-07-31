open! Core
open! Async_kernel
open! Async_rpc_kernel

(* open Async_js *)
open Bonsai_web

(* open Async_kernel.Let_syntax *)
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let generate_player_string () =
  let random_offset = Random.int 500 in
  let player_number = random_offset + 1 in
  Printf.sprintf "Player %d" player_number
;;

let loading_page = {%html|
  <p>Loading...</p>
|}

let serve_route (local_ graph) =
  (* let route = Url_var.value url_var in *)
  let initialized, toggle_initialized =
    Bonsai.toggle ~default_model:false graph
  in
  let player_name = generate_player_string () in
  let player_name_as_query =
    Rpcs.Client_message.Query.New_player player_name
  in
  let dispatch_new_player =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Client_message.rpc
      graph
      ~where_to_connect:
        (Bonsai.return
           (Rpc_effect.Where_to_connect.self ~on_conn_failure:Rpc_effect.On_conn_failure.Surface_error_to_rpc ()))
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr dispatch_new_player and _t = toggle_initialized in
       print_endline "did stuff";
       let%bind.Effect () = Effect.print_s [%sexp "hi"] in
       let%bind.Effect _a = dispatch_new_player player_name_as_query in
       let%bind.Effect () = Effect.print_s [%sexp "hiya"] in
       Ui_effect.return ()
       )
    graph;
  (* let%bind.Effect he = dispatch_new_player (Rpcs.Client_message.Query.New_player player_name) *)
  let response =
    Rpc_effect.Polling_state_rpc.poll
      Rpcs.Poll_client_state.rpc
      ~equal_query:[%equal: Rpcs.Poll_client_state.Query.t]
      ~every:(Bonsai.return (Time_ns.Span.of_sec 0.1))
      ~where_to_connect:
        (Bonsai.return
           (Rpc_effect.Where_to_connect.self
              ~on_conn_failure:Rpc_effect.On_conn_failure.Surface_error_to_rpc
              ()))
      (Bonsai.return { Rpcs.Poll_client_state.Query.name = player_name })
      graph
  in
  let current_state =
    let%arr response in
    let error = response.last_error in
    let response = response.last_ok_response in
    match response, error with
    | None, Some (_, err) -> failwith (Error.to_string_hum err)
    | Some (_, current_state), _ -> Some current_state
    | None, _ -> None
  in
  match%sub current_state with
  | None -> Bonsai.return loading_page
  | Some current_state ->
    let%sub { current_phase; _ } = current_state in
    (match%sub current_phase, initialized with
     | _, false -> Bonsai.return loading_page
     | Waiting_room, true -> Pages.Waiting_room.page current_state graph
     | Rules, true -> Pages.Rules.body ()
     | Negotiation, true -> Pages.Negotiation.body current_state graph
     | _ -> Pages.Waiting_room.page current_state graph)
;;

(* let connector =
    Rpc_effect.Connector.persistent_connection
      ~on_conn_failure:Rpc_effect.On_conn_failure.Retry_until_success
      ~connection_state:(fun conn -> (), conn) *)

let () = Bonsai_web.Start.start serve_route