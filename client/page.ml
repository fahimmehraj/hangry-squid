open! Core
open! Bonsai_web
module Url_var = Bonsai_web_ui_url_var


type t = Waiting_room | Item_selection | Negotiation | Action | Outcome [@@deriving sexp, equal]

let parse_exn ({ path; _  }: Url_var.Components.t) : t =
  match path with
  | "/" -> Waiting_room
  | _ -> Negotiation
;;

let unparse t :Url_var.Components.t =
  match t with | Waiting_room -> Url_var.Components.create ~path:"/" ()
  | _ -> Url_var.Components.create ~path:"/chat" ()