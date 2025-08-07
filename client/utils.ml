open! Core
open! Hangry_squid
open! Async_kernel
open! Async_rpc_kernel
open! Bonsai_web

let dispatcher (local_ graph) =
  Rpc_effect.Rpc.dispatcher
    Rpcs.Client_message.rpc
    graph
    ~where_to_connect:
      (Bonsai.return
         (Rpc_effect.Where_to_connect.self
            ~on_conn_failure:Rpc_effect.On_conn_failure.Surface_error_to_rpc
            ()))
;;
