open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var

let serve_route (url_var: Page.t Url_var.t) (local_ graph) =
  let route = Url_var.value url_var in
  match%sub route with
  | Waiting_room -> Waiting_room.page url_var graph
  | Rules -> Rules.body url_var
  | Negotiation -> Negotiation.body url_var
  | _ -> Waiting_room.page url_var graph
;;

let url_var = Url_var.create_exn (module Page) ~fallback:Waiting_room

let () = Bonsai_web.Start.start (serve_route url_var)
