open! Core
open! Sexplib.Std (* so we can have access to string_of_sexp *)

type t =
  { player_in_question : string
  ; message : string
  }
[@@deriving sexp, bin_io]
