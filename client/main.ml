(* write .mli files for pages *)
open! Core
open! Async_kernel
open! Async_rpc_kernel
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid

let loading_page = {%html|
  <p>Loading...</p>
|}

let page_with_header (current_state : Client_state.t Bonsai.t) (local_ graph)
  =
  let%sub { round_start; current_phase; me; _ } = current_state in
  let%sub { is_alive; _ } = me in
  match%sub is_alive with
  | false -> Pages.Death.body graph
  | true ->
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
        query
        graph
    in
    let%sub { last_error; last_ok_response; _ } = result in

    (* Float errors to alerts *)
    Bonsai.Edge.on_change
      ~equal:
        (Option.equal (fun (_, error1) (_, error2) ->
           Error.equal error1 error2))
      ~callback:
        (Bonsai.return (fun err ->
           match err with
           | None -> Effect.Ignore
           | Some (query, err) ->
             Effect.alert
               (Sexp.to_string
                  [%message
                    "Websocket error"
                      ~query:(query : Rpcs.Poll_client_state.Query.t)
                      ~error:(err : Error.t)])))
      last_error
      graph;
    (* do something with Error types? *)
    match%sub last_ok_response with
    | None -> Bonsai.return None
    | Some resp ->
      let%arr resp in
      Some (snd resp)
  in
  match%sub client_state with
  | None -> Bonsai.return loading_page
  | Some client_state -> page_with_header client_state graph
;;

let serve_route (local_ graph) =
  let join_game_state, update_join_game_state =
    Pages.Landing.join_status_state_machine graph
  in
  match%sub join_game_state with
  | Accepted_name name -> render_game_page name graph
  | Entering_name (name, error) ->
    Pages.Landing.body update_join_game_state error
;;

let () = Bonsai_web.Start.start serve_route
