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
  ; current_phase = Negotiation
  ; players
  ; ready_players = [ "Player 2" ; "Player 3"]
  ; public_messages = []
  ; my_messages = String.Map.empty
  ; public_results = []
  ; my_results = []
  ; item_choices = Some (Item.pocket_knife, Item.medical_kit)
  ; me = { name = "Player 1" ; is_alive = true ; inventory = [ Item.pocket_knife ] ; health = 100}
  }
;;

let serve_route (current_state : Client_state.t Bonsai.t) (local_ graph) =
  (* let route = Url_var.value url_var in *)
  let%sub { current_phase; _ } = current_state in
  match%sub current_phase with
    | Waiting_room -> Pages.Waiting_room.page current_state graph
    | Rules -> Pages.Rules.body ()
    | Negotiation -> Pages.Negotiation.body current_state graph
    | _ -> Pages.Waiting_room.page current_state graph
    (* | Rules -> Rules.body graph
    | Item_selection -> Select.body graph
    | Negotiation -> Negotiation.body graph
    | Round_results -> Outcome.body graph
    | Game_results -> Game_over.body graph
    | Item_usage -> Use_item.body graph *)
;;

(* let url_var = Url_var.create_exn (module Page) ~fallback:Waiting_room *)
let current_state = Bonsai.return _dummy_state
let () = Bonsai_web.Start.start (serve_route current_state)
