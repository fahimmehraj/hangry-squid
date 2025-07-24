open! Core

type t =
  { user : Player.t
  ; recipient : Player.t
  ; item_used : Item.t
  }
[@@deriving sexp]
