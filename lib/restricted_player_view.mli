open! Core

type t =
  { health : int
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp]

val equal : t -> t -> bool
val of_player : Player.t -> t
