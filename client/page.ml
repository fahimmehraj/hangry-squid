open! Core
open! Bonsai_web
module Url_var = Bonsai_web_ui_url_var

include Hangry_squid.Game_phase

let parse_exn ({ path; _ } : Url_var.Components.t) : t =
  print_endline path;
  match path with
  | "" | "/" -> Waiting_room
  | "rules" -> Rules
  | "select" -> Item_selection
  | "negotiation" -> Negotiation
  | "use_item" -> Item_usage
  | "outcome" -> Round_results
  | "game_over" -> Game_results
  | _ -> failwith "Unknown Path"
;;

let unparse t : Url_var.Components.t =
  match t with
  | Waiting_room -> Url_var.Components.create ~path:"/" ()
  | Rules -> Url_var.Components.create ~path:"rules" ()
  | Item_selection -> Url_var.Components.create ~path:"select" ()
  | Negotiation -> Url_var.Components.create ~path:"negotiation" ()
  | Item_usage -> Url_var.Components.create ~path:"use_item" ()
  | Round_results -> Url_var.Components.create ~path:"outcome" ()
  | Game_results -> Url_var.Components.create  ~path:"game_over" ()
;;
