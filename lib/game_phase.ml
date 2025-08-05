type t = 
  | Waiting_room
  | Rules
  | Item_selection
  | Negotiation
  | Item_usage
  | Round_results
  | Game_results
[@@deriving sexp, bin_io, equal]

let to_string = function
| Waiting_room -> "Waiting Room"
| Rules -> "Rules"
| Item_selection -> "Item Selection"
| Negotiation -> "Negotiation"
| Item_usage -> "Use Item"
| Round_results -> "Outcome"
| Game_results -> "Game Results"

let to_duration t =
  match t with 
  | Rules -> 1
  | Item_selection -> 1
  | Negotiation -> 1
  | Item_usage -> 100
  | Round_results -> 5
  | Game_results -> 15
  | Waiting_room -> Int.max_int
;;
