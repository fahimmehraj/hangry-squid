open! Core

type t =
  { current_round : int
  ; current_phase : Game_phase.t
  ; players : Player.t String.Map.t
  ; ready_players : string list
  ; actions_taken_in_round : Action.t list
  ; public_messages : Message.t list
  ; private_messages : Message.t list String.Map.t String.Map.t
  ; public_results : Round_result.t list
  ; private_results : Round_result.t list String.Map.t
  ; item_choices_by_user : (Item.t * Item.t) option String.Map.t
  ; round_start : Time_ns.t
  }
[@@deriving sexp]

val create_empty_game : unit -> t
val get_client_state_from_name : t -> string -> Client_state.t
val name_taken : t -> string -> bool
val ready_player : t -> Rpcs.Client_message.Ready_status_change.t -> t
val add_item_to_inventory : t -> Rpcs.Client_message.Item_selection.t -> t
val add_public_message : t -> Message.t -> t
val add_message : t -> Message.t -> t
val add_player : t -> Player.t -> t
val add_action : t -> Action.t -> t
val apply_actions_taken : t -> t
val compile_all_elimination_results : t -> t
val players_left : t -> int
