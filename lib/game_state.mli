open! Core

type t =
  { current_round : int
  ; current_phase : Game_phase.t
  ; players : Player.t String.Map.t
  ; ready_players : string list
  ; actions_taken_in_round : Action.t list
  ; public_message : Message.t list
  ; private_messages : Message.t list String.Map.t
  ; public_results : Round_result.t list
  ; private_results : Round_result.t list String.Map.t
  }
[@@deriving sexp]

val create_empty_game : unit -> t
val get_private_messages_by_user : t -> string -> (Message.t list) String.Map.t
val get_private_results_by_user : t -> Round_result.t list
val name_staken : t -> string -> bool
val ready_player : t -> Rpcs.Client_ready.Query.t -> t
val add_player : t -> Player.t -> t
val add_action : t -> Action.t -> t
val apply_actions_taken : t -> t
val compile_all_elimination_results : t -> t
val players_left : t -> int
