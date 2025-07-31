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

let page_with_header (current_state : Client_state.t Bonsai.t) (local_ graph)
  =
  let%sub { round_start; current_phase; me; _ } = current_state in
  let now =
    Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph
  in
  let header =
    let%arr now and round_start and me and current_phase in
    let seconds_left =
      Time_ns.abs_diff now round_start |> Time_ns.Span.to_int_sec
    in
    Components.header
      me
      ~phase_name:(Game_phase.to_string current_phase)
      ~seconds_left
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
           (Rpc_effect.Where_to_connect.self
              ~on_conn_failure:
                Rpc_effect.On_conn_failure.Surface_error_to_rpc
              ()))
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr dispatch_new_player and toggle_initialized in
       print_endline "did stuff";
       let%bind.Effect () = Effect.print_s [%sexp "hi"] in
       let%bind.Effect _a = dispatch_new_player player_name_as_query in
       let%bind.Effect () = Effect.print_s [%sexp "hiya"] in
       toggle_initialized)
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
              ~on_conn_failure:
                Rpc_effect.On_conn_failure.Surface_error_to_rpc
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
    | Some (_, current_state), _ ->
      print_s [%sexp (current_state : Client_state.t)];
      Some current_state
    | None, _ -> None
  in
  match%sub current_state with
  | None -> Bonsai.return loading_page
  | Some current_state ->
    let%sub { current_phase; _ } = current_state in
    (match%sub initialized with
     | false -> Bonsai.return loading_page
     | true -> page_with_header current_state graph)
;;

(* let connector =
    Rpc_effect.Connector.persistent_connection
      ~on_conn_failure:Rpc_effect.On_conn_failure.Retry_until_success
      ~connection_state:(fun conn -> (), conn) *)

let () = Bonsai_web.Start.start serve_route
