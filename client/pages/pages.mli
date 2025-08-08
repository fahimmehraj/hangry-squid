open! Core
open! Bonsai_web
open! Hangry_squid
open! Async_kernel
open! Async_rpc_kernel

module Landing : sig
  val join_status_state_machine
    :  unit Ui_effect.t Bonsai.t
    -> local_ Bonsai.graph
    -> Landing_state.t Bonsai.t
       * ([< `Ack_join of string
          | `Failed_join of string
          | `Try_to_join_game
          | `Update_name of string > `Ack_join
          `Failed_join
          ]
          -> unit Ui_effect.t)
           Bonsai.t

  val body
    :  ([> `Try_to_join_game | `Update_name of string ] -> unit Ui_effect.t)
         Bonsai.t
    -> string option Bonsai.t
    -> Vdom.Node.t Bonsai.t
end

module Waiting_room : sig
  val body
    :  Client_state.t Bonsai.t
    -> local_ Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end

module Rules : sig
  val body : unit -> Vdom.Node.t Bonsai.t
end

module Select : sig
  val body
    :  Client_state.t Bonsai.t
    -> local_ Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end

module Negotiation : sig
  val body
    :  Client_state.t Bonsai.t
    -> local_ Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end

module Use_item : sig
  val body
    :  Client_state.t Bonsai.t
    -> local_ Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end

module Outcome : sig
  val body
    :  Client_state.t Bonsai.t
    -> local_ Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end

module Game_over : sig
  val body
    :  Client_state.t Bonsai.t
    -> local_ Bonsai.graph
    -> Vdom.Node.t Bonsai.t
end

module Death : sig
  val body : unit -> Vdom.Node.t Bonsai.t
end
