open! Core

type t = {
  action_user : Player.t;
  action_recipient : Player.t;
  item_used : Item.t;
}
[@@deriving sexp]
