open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let players =
  List.init 8 ~f:(fun i -> Printf.sprintf "Player %d" (i + 1))
  |> List.map ~f:(fun name ->
    { Restricted_player_view.name; health = 75; is_alive = true })
;;

let _dummy_state : Client_state.t =
  { current_round = 1
  ; current_phase = Waiting_room
  ; my_inventory = [ Item.pocket_knife ]
  ; players
  ; ready_players = List.map players ~f:(fun p -> p.name)
  ; public_messages = []
  ; my_messages = String.Map.empty
  ; public_results = []
  ; my_results = []
  ; item_choices = Item.pocket_knife, Item.medical_kit
  }
;;

let serve_route (url_var : Page.t Url_var.t) (local_ graph) =
  let route = Url_var.value url_var in
  match%sub route with
  | Waiting_room -> Waiting_room.page url_var graph
  | Rules -> Rules.body url_var
  | Item_selection -> Select.body url_var
  | Negotiation -> Negotiation.body url_var
  | Round_results -> Outcome.body url_var
  | Game_results -> Game_over.body url_var
  | Item_usage -> Use_item.body url_var
;;

let url_var = Url_var.create_exn (module Page) ~fallback:Waiting_room
let () = Bonsai_web.Start.start (serve_route url_var)
