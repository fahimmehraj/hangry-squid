open! Core

type t = {
  health : int; (* maybe restricted*)
  inventory : Item.t list;
  is_alive : bool;
  name : string;
}
[@@deriving sexp]

val new_player : string -> t
