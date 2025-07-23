open! Core

type t = {
  health : int; (* maybe restricted *)
  inventory : Item.t list;
  is_alive : bool;
  name : string;
}
[@@deriving sexp]

let default_health = 100
let default_inventory = []

let new_player name =
  { health = default_health ; inventory = default_inventory ; is_alive = true ; name }
