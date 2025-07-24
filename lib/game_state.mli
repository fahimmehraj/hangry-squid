open! Core

type t [@@deriving sexp]

val create_empty_game : unit -> t
val add_action : t -> Action.t -> t
val apply_actions_taken : t -> t
