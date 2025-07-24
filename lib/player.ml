open! Core

module T = struct
  type t =
    { health : int (* maybe restricted *)
    ; inventory : Item.t list
    ; is_alive : bool
    ; name : string
    }
  [@@deriving sexp, compare]
end

include T

module PlayerMap = Map.Make(T)

let default_health = 100
let default_inventory = []

let equal t1 t2 =
  String.equal t1.name t2.name
;;

let new_player name =
  { health = default_health
  ; inventory = default_inventory
  ; is_alive = true
  ; name
  }
;;
