open! Core

type t =
  | Observer
  | Item_interception
  | Medical_kit of Item_effect.t
  | Poisonous_dart of Item_effect.t
  | Pocket_knife of Item_effect.t
  | Gamblers_potion of Item_effect.t
[@@deriving sexp, compare]

val observer : t
val item_interception : t
val medical_kit : t
val poisonous_dart : t
val pocket_knife : t
val gamblers_potion : t
val to_string : t -> string
val equal : t -> t -> bool
val get_two_random_items_no_duplicates : unit -> t * t
