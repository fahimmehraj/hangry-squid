open! Core

type t [@@deriving sexp]

val create_empty_game : unit -> t
val add_player : t -> Player.t -> t
val add_action : t -> Action.t -> t
val apply_actions_taken : t -> t
val compile_all_elimination_results : t -> t
