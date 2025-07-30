open! Core

type t =
  { sender : string
  ; recipient : string option
  ; contents : string
  ; timestamp : Time_ns.t
  }
[@@deriving sexp, bin_io, equal]
