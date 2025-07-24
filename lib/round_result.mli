open! Sexplib.Std (* so we can have access to string_of_sexp *)

type t =
  { player_in_question : Player.t
  ; message : string
  }
[@@deriving sexp]
