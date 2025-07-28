open Hangry_squid

type t = {
  mutable game_state : Game_state.t
; mutable rpc_pipes : 'a String.Map.t
}
[@@deriving sexp]

let initialize_server_state () = 
  {
    game_state = Game_state.create_empty_game ()
    ; rpc_pipes = Map.empty (module String)
    ; ready_players = []
  }
