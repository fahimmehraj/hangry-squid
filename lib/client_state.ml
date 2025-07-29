open Core

(* add timestamp for when the phase started *)
(* add representation of previous results for *)
type t =
  { current_round : int
  ; current_phase : Game_phase.t
  ; my_inventory : Item.t list
  ; players : Restricted_player_view.t list
  ; ready_players : string list
  ; public_messages : Message.t list
  ; my_messages : Message.t list String.Map.t
  ; public_results : Round_result.t list
  ; my_results : Round_result.t list
  ; item_choices : (Item.t * Item.t) option
  ; my_name : string
  }
[@@deriving sexp, bin_io]
