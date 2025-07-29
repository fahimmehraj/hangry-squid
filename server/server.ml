open! Core
open Async
open Hangry_squid

let write_client_states_to_all (server_state_ref : Server_state.t ref) =
  Map.iteri !server_state_ref.rpc_pipes ~f:(fun ~key ~data ->
    let player_name = key in
    let writer = data in
    let client's_state =
      Game_state.get_client_state_from_name
        !server_state_ref.game_state
        player_name
    in
    Pipe.write_without_pushback_if_open writer client's_state)
;;

let add_pipe_rpc_to_state
  (server_state_ref : Server_state.t ref)
  (name : string)
  =
  let reader, writer = Pipe.create () in
  !server_state_ref.rpc_pipes
  <- Map.set !server_state_ref.rpc_pipes ~key:name ~data:writer;
  write_client_states_to_all server_state_ref;
  Ok reader
;;

let handle_client_requesting_pipe
  (query : Rpcs.State_pipe.Query.t)
  (server_state_ref : Server_state.t ref)
  =
  let name = query.name in
  match Game_state.name_taken !server_state_ref.game_state name with
  | true ->
    (match Map.find !server_state_ref.rpc_pipes name with
     | Some pipe ->
       if Pipe.is_closed pipe
       then add_pipe_rpc_to_state server_state_ref name
       else Error "Name already taken"
     | None -> add_pipe_rpc_to_state server_state_ref name)
  | false -> add_pipe_rpc_to_state server_state_ref name
;;

let sleep seconds =
  let%bind () = Clock_ns.after (Time_ns.Span.of_int_sec seconds) in
  return ()
;;

let change_game_phase (server_state_ref : Server_state.t ref) phase =
  !server_state_ref.game_state
  <- { !server_state_ref.game_state with
       current_phase = phase
     ; round_start = Time_ns.now ()
     }
;;

let rec handle_round (server_state_ref : Server_state.t ref) ~(round : int)
  : unit Deferred.t
  =
  !server_state_ref.game_state
  <- { !server_state_ref.game_state with current_round = round };
  change_game_phase server_state_ref Game_phase.Rules;
  let%bind () = sleep (Game_phase.to_duration Game_phase.Rules) in
  change_game_phase server_state_ref Game_phase.Item_selection;
  let%bind () = sleep (Game_phase.to_duration Game_phase.Item_selection) in
  change_game_phase server_state_ref Game_phase.Negotiation;
  let%bind () = sleep (Game_phase.to_duration Game_phase.Negotiation) in
  change_game_phase server_state_ref Game_phase.Item_usage;
  let%bind () = sleep (Game_phase.to_duration Game_phase.Item_usage) in
  change_game_phase server_state_ref Game_phase.Round_results;
  let%bind () = sleep (Game_phase.to_duration Game_phase.Round_results) in
  let players_left = Game_state.players_left !server_state_ref.game_state in
  if players_left > 0
  then handle_round server_state_ref ~round:(round + 1) |> don't_wait_for
  else change_game_phase server_state_ref Game_phase.Game_results;
  return ()
;;

let handle_ready_message
  (server_state_ref : Server_state.t ref)
  (query : Rpcs.Client_message.Ready_status_change.t)
  : Rpcs.Client_message.Response.t
  =
  match Game_state.name_taken !server_state_ref.game_state query.name with
  | true ->
    !server_state_ref.game_state
    <- Game_state.ready_player !server_state_ref.game_state query;
    (* check if everyone is ready *)
    handle_round server_state_ref ~round:1 |> don't_wait_for;
    Ok "OK"
  | false -> Error "Player name isn't registered"
;;

let handle_item_selection
  (server_state_ref : Server_state.t ref)
  (query : Rpcs.Client_message.Item_selection.t)
  : Rpcs.Client_message.Response.t
  =
  match
    Game_phase.equal
      Game_phase.Item_selection
      !server_state_ref.game_state.current_phase
  with
  | true ->
    !server_state_ref.game_state
    <- Game_state.add_item_to_inventory !server_state_ref.game_state query;
    Ok "OK"
  | false -> Error "It is not currently the item selection phase"
;;

let handle_message
  (server_state_ref : Server_state.t ref)
  (message : Message.t)
  : Rpcs.Client_message.Response.t
  =
  match
    Game_phase.equal
      Game_phase.Negotiation
      !server_state_ref.game_state.current_phase
  with
  | true ->
    !server_state_ref.game_state
    <- Game_state.add_message !server_state_ref.game_state message;
    Ok "OK"
  | false -> Error "It is not currently the negotiation phase"
;;

let handle_item_used
  (server_state_ref : Server_state.t ref)
  (action : Action.t)
  : Rpcs.Client_message.Response.t
  =
  match
    Game_phase.equal
      Game_phase.Item_usage
      !server_state_ref.game_state.current_phase
  with
  | true ->
    !server_state_ref.game_state
    <- Game_state.add_action !server_state_ref.game_state action;
    Ok "OK"
  | false -> Error "It is not currently the item usage phase"
;;

let handle_client_message
  (query : Rpcs.Client_message.Query.t)
  (server_state_ref : Server_state.t ref)
  =
  let response =
    match query with
    | Ready_status_change status_change ->
      handle_ready_message server_state_ref status_change
    | Item_selection item_selection ->
      handle_item_selection server_state_ref item_selection
    | Chat_message message -> handle_message server_state_ref message
    | Item_used action -> handle_item_used server_state_ref action
  in
  (* make sure that all clients have the newest game state *)
  write_client_states_to_all server_state_ref;
  response
;;

let start_server port server_state_ref =
  let%bind server =
    Rpc.Connection.serve
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_exception:Rpc.On_exception.Close_connection
           ~on_unknown_rpc:`Close_connection
           ~implementations:
             [ Rpc.Rpc.implement Rpcs.Client_message.rpc (fun _ query ->
                 return (handle_client_message query server_state_ref))
             ; Rpc.Pipe_rpc.implement Rpcs.State_pipe.rpc (fun _ query ->
                 return
                   (handle_client_requesting_pipe query server_state_ref))
             ])
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  Tcp.Server.close_finished server
;;

let start_server_command =
  Command.async
    ~summary:"Start a game server"
    (let%map_open.Command server_port =
       flag "port" (required int) ~doc:"server <port> number"
     in
     let authoritative_game_state =
       ref (Server_state.initialize_server_state ())
     in
     fun () -> start_server server_port authoritative_game_state)
;;
