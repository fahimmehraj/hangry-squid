open! Core

type t =
  { user : string
  ; recipient : string
  ; item_used : Item.t
  }
[@@deriving sexp]
