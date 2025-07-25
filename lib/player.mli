open! Core

type t =
  { health : int 
  ; inventory : Item.t list
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp]

val equal : t -> t -> bool
val new_player : string -> t
