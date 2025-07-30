open! Core

type t =
  { health : int
  ; inventory : Item.t list
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp, bin_io]

val equal : t -> t -> bool
val new_player : string -> t
