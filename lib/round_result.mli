open! Sexplib.Std (* so we can have access to string_of_sexp *)

type t =
  { relevant_player : Player.t
  ; message : string
  }
[@@deriving sexp]
