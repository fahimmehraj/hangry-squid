open! Core
open Async
open Hangry_squid

let handle_ready_message
      (query : Rpcs.Client_ready.Query.t)
      (server_state : Server_state.t)
  : Rpcs.Client_ready.Response.t
  =
  match Game_state.name_taken server_state.game_state query.name with
  | true ->
    server_state.game_state
    <- Game_state.ready_player server_state.game_state query;
    Ok "OK"
  | false -> Error "Player name isn't registered"
;;

let handle_client_connecting (query : Rpcs.Client_connecting.Query.t) (server_state : Server_state.t)
  : Rpcs.Client_connecting.Response.t
  =
  match Game_state.name_taken server_state.game_state query.name with 
  | true -> 
    Error "TODO"
  | false ->
    Rpc.Pipe_rpc.create ~name:"server-message" ~version:0
;;

let start_server port server_state =
  let%bind server =
    (* Rpc.Connection.create *)
    Rpc.Connection.serve
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_unknown_rpc:`Close_connection
           ~implementations:
             [ Rpc.Rpc.implement Rpcs.Client_ready.rpc (fun _ query ->
                 return (handle_ready_message query server_state))
             ; Rpc.Rpc.implement Rpcs.Client_connecting.rpc (fun _ query ->
                 return (handle_client_connecting query server_state))
             ])
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  (* Rpc.Connection.close server *)
  Tcp.Server.close_finished server
;;

let start_server_command =
  Command.async
    ~summary:"Start a game server"
    (let%map_open.Command server_port =
       flag "port" (required int) ~doc:"server <port> number"
     in
     let authoritative_game_state = ref (Game_state.create_empty_game ()) in
     fun () -> start_server server_port authoritative_game_state)
;;
