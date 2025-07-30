open! Core

type t =
  { health : int
  ; inventory : Item.t list
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp, bin_io]

let default_health = 100
let default_inventory = []
let equal t1 t2 = String.equal t1.name t2.name

let new_player name =
  { health = default_health
  ; inventory = default_inventory
  ; is_alive = true
  ; name
  }
;;
