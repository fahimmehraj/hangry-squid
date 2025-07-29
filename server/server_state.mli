open! Core
open Hangry_squid

type t =
  { mutable game_state : Game_state.t
  ; mutable rpc_pipes : 'a String.Map.t
  }
[@@deriving sexp]

val initilize_server_state : unit -> t
