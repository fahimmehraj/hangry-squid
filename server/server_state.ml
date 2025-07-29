open! Core
open Async
open Hangry_squid

type t =
  { mutable game_state : Game_state.t
  ; mutable rpc_pipes : Client_state.t Pipe.Writer.t String.Map.t
  }
[@@deriving sexp_of]

let initialize_server_state () =
  { game_state = Game_state.create_empty_game ()
  ; rpc_pipes = Map.empty (module String)
  }
;;
