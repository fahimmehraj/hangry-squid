open Server

let command_group =
  Command.group ~summary:"Hangry Games"
    [
      ("start-server", start_server_command);
      (* ("join-server", join_server_command); *)
    ]

let () = Command_unix.run command_group
