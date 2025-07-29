open! Core

type t = {
  add_health : int;
  chance_of_adding : float;
  remove_health : int;
  chance_of_removing : float;
}
[@@deriving sexp, bin_io]

let create ?(add_health = 0) ?(chance_of_adding = 0.0) ?(remove_health = 0)
    ?(chance_of_removing = 0.0) () : t =
  { add_health; chance_of_adding; remove_health; chance_of_removing }
