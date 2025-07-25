open! Core
open! Bonsai_web
module Url_var = Bonsai_web_ui_url_var

type t =
  | Waiting_room
  | Rules
  | Item_selection
  | Negotiation
  | Action
  | Outcome
[@@deriving sexp, equal]

let parse_exn ({ path; _ } : Url_var.Components.t) : t =
  print_endline path;
  match path with
  | "client/" -> Waiting_room
  | "client/rules/" -> Rules
  | "client/negotiation" -> Negotiation
  | _ -> Waiting_room
;;

let unparse t : Url_var.Components.t =
  match t with
  | Waiting_room -> Url_var.Components.create ~path:"client/" ()
  | Rules -> Url_var.Components.create ~path:"client/rules/" ()
  | Negotiation -> Url_var.Components.create ~path:"client/negotiation" ()
  | _ -> Url_var.Components.create ~path:"client/whatever" ()
;;
