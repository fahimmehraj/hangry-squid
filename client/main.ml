open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var

let serve_route (route: Page.t Url_var.t) (local_ graph) =
  let route = Url_var.value route in
  match%sub route with
  | _ -> Waiting_room.page route graph
;;

let url_var = Url_var.create_exn (module Page) ~fallback:Waiting_room

let () = Bonsai_web.Start.start (serve_route url_var)
