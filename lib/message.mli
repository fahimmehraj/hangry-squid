open! Core
(* open! Core_unix *)

type t =
  { sender : string
  ; recipient : string option
  ; contents : string
  ; timestamp : Time_ns.t
  }
[@@deriving sexp]
