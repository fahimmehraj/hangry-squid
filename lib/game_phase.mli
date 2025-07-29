type t = 
  | Waiting_room
  | Rules
  | Item_selection
  | Negotiation
  | Item_usage
  | Round_results
  | Game_results
[@@deriving sexp, bin_io, equal]

val to_duration : t -> int
