open! Core

type t =
  { health : int
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp, bin_io]

val equal : t -> t -> bool
val of_player : Player.t -> t
