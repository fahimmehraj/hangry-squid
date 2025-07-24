open! Core

type t = {
  add_health : int;
  chance_of_adding : float;
  remove_health : int;
  chance_of_removing : float;
}
[@@deriving sexp, compare]

val create :
  ?add_health:int ->
  ?chance_of_adding:float ->
  ?remove_health:int ->
  ?chance_of_removing:float ->
  unit ->
  t
