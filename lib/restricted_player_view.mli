open! Core

type t =
  { health : int
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp, bin_io, compare]

val equal : t -> t -> bool
val of_player : Player.t -> t

include Comparator.S with type t := t
