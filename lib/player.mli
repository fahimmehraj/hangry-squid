open! Core

type t =
  { health : int (* maybe restricted*)
  ; inventory : Item.t list
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp, compare]

(* module Map : Map.S with type Key.t = t *)

val equal : t -> t -> bool
val new_player : string -> t
