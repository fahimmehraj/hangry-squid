open! Core
open Async

let handle_send_message query server_state =
  let recipient = query.recipient in
  let alice_pipe = Server_state.find_pipe_for server_state ~recipient  in

;;

let start_server port server_state = 
  let%bind server = 
    Rpc.Connection.serve 
      ~implementations:(
        Rpc.Implementations.create_exn ~on_unknown_rpc:`Close_connection
        ~implementations: 
          [
            Rpc.Rpc.implement My_rpc.Send_message (fun query -> 
              handle_send_message query server_state
              )
              ;Rpc.State_rpc

          ]
      )
      ~initial_connection_state:(fun _ _ -> ())
      ~where_to_list:(Tcp.Where_to_listen.of_port port)
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
     fun () -> start_server server_port)
;;
