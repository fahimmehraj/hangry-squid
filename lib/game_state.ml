open! Core

type t =
  { current_round : int
  ; players : Player.t String.Map.t
    (* map from player name to a player type, names should be unique*)
  ; actions_taken_in_round : Action.t list
  ; public_results : Round_result.t list
  ; private_results : (Round_result.t list) String.Map.t
  }
(* [@@deriving sexp] *)

let create_empty_game () =
  { current_round = 0
  ; players = Map.empty (module String)
  ; actions_taken_in_round = []
  ; public_results = []
  ; private_results = Map.empty (module String)
  }
;;

let add_action t (action : Action.t) =
  { t with actions_taken_in_round = action :: t.actions_taken_in_round }
;;

let get_items_used_by_player t (player : Player.t) =
  List.filter_map t.actions_taken_in_round ~f:(fun action ->
    if Player.equal action.user player then Some action.item_used else None)
;;

let add_private_result t player = 
  let 

let apply_actions_taken t =
  List.fold t.actions_taken_in_round ~init:t ~f:(fun acc_state action_taken ->
     let user = action_taken.user
     and recipient = action_taken.recipient in

     match action_taken.item_used with
     | Item.Observer ->
      let actions_observed = get_items_used_by_player acc_state recipient in
      let actions_observed_as_string = List.map actions_observed ~f:Item.to_string |> String.concat ~sep:", " in
      let inventory_observed = recipient.inventory in
      let inventory_as_string = List.map inventory_observed ~f:Item.to_string |> String.concat ~sep:", " in
      let message = "Inventory Observed: " ^ inventory_as_string  ^ "\nActions Observed: " ^ actions_observed_as_string in
      let new_result = {
        relevant_player = recipient ; message
      }

      { acc_state with Map.set acc_state.private_results ~key:}
  )
;;

let compile_all_results t = ()
