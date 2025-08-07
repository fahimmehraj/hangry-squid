open! Core
open! Hangry_squid
open! Async_kernel
open! Async_rpc_kernel

val dispatcher
  :  local_ Bonsai.graph
  -> (Rpcs.Client_message.Query.t
      -> Rpcs.Client_message.Response.t Or_error.t Ui_effect.t)
       Bonsai.t
