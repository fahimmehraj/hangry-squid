type t = 
  | Waiting_room
  | Rules
  | Item_selection
  | Negotiation
  | Item_usage
  | Round_results
  | Game_results
[@@deriving sexp, bin_io, equal]

let to_duration t =
  match t with 
  | Rules -> 20
  | Item_selection -> 15
  | Negotiation -> 60
  | Item_usage -> 15
  | Round_results -> 20
  | Waiting_room | Game_results -> Int.max_int
;;
