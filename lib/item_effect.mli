open! Core

type t = {
  add_health : int;
  chance_of_adding : float;
  remove_health : int;
  chance_of_removing : float;
}
[@@deriving sexp]

val create :
  ?add_health:int ->
  ?chance_of_adding:float ->
  ?remove_health:int ->
  ?chance_of_removing:float ->
  unit ->
  t
