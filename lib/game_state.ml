open! Core

type t =
  { current_round : int
  ; players : Player.t String.Map.t
    (* map from player name to a player type, names should be unique*)
  ; actions_taken_in_round : Action.t list
  ; public_results : Round_result.t list
  ; private_results : Round_result.t list String.Map.t
  }
[@@deriving sexp]

let create_empty_game () =
  { current_round = 0
  ; players = Map.empty (module String)
  ; actions_taken_in_round = []
  ; public_results = []
  ; private_results = Map.empty (module String)
  }
;;

let add_player t (player : Player.t) =
  match Map.find t.players player.name with
  | None ->
    { t with players = Map.set t.players ~key:player.name ~data:player }
  | Some _ -> failwith "Name already taken"
;;

let add_action t (action : Action.t) =
  { t with actions_taken_in_round = action :: t.actions_taken_in_round }
;;

let get_items_used_by_player t player =
  List.filter_map t.actions_taken_in_round ~f:(fun action ->
    if String.equal action.user player then Some action.item_used else None)
;;

let add_private_result t player_name result =
  let new_private_results =
    match Map.find t.private_results player_name with
    | None -> [ result ]
    | Some results_list -> result :: results_list
  in
  { t with
    private_results =
      Map.set t.private_results ~key:player_name ~data:new_private_results
  }
;;

let add_observer_message t (action : Action.t) =
  let user = Map.find_exn t.players action.user
  and recipient = Map.find_exn t.players action.recipient in
  let actions_observed = get_items_used_by_player t recipient.name in
  let actions_observed_as_string =
    List.map actions_observed ~f:Item.to_string |> String.concat ~sep:", "
  in
  let inventory_as_string =
    List.map recipient.inventory ~f:Item.to_string |> String.concat ~sep:", "
  in
  let message =
    [%string
      "Inventory Observed: %{inventory_as_string}\n\
       Actions Observed: %{actions_observed_as_string}"]
  in
  let new_result : Round_result.t =
    { player_in_question = recipient.name; message }
  in
  add_private_result t user.name new_result
;;

let add_item_interception_message t (action : Action.t) =
  let user = Map.find_exn t.players action.user
  and recipient = Map.find_exn t.players action.recipient in
  let user_result : Round_result.t =
    { player_in_question = recipient.name
    ; message =
        [%string "will not receive an item next round because of you"]
    }
  in
  let new_game_state = add_private_result t user.name user_result in
  (* we don't want to send the same message to the same user in case they happened to waste an item on themselves *)
  match Player.equal user recipient with
  | true -> new_game_state
  | false ->
    let recipient_result : Round_result.t =
      { player_in_question = user.name
      ; message = [%string "blocked you from receiving an item next round"]
      }
    in
    add_private_result t recipient.name recipient_result
;;

let add_gamblers_potion_message t (action : Action.t) health_change =
  let user = Map.find_exn t.players action.user
  and recipient = Map.find_exn t.players action.recipient in
  let (recipient_result : Round_result.t), (user_result : Round_result.t) =
    match health_change with
    | 60 ->
      ( { player_in_question = user.name
        ; message =
            [%string
              "used Gambler's Potion on you, adding %{health_change#Int}HP \
               to you"]
        }
      , { player_in_question = recipient.name
        ; message = [%string "gained %{health_change#Int}HP because of you"]
        } )
    | 40 ->
      ( { player_in_question = user.name
        ; message =
            [%string
              "used Gmabler's Potion on you, removing \
               %{health_change#Int}HP from you"]
        }
      , { player_in_question = recipient.name
        ; message = [%string "lost %{health_change#Int}HP because of you"]
        } )
    | _ -> failwith "add_gamblers_potion_message: invalid health_change"
  in
  let new_game_state =
    add_private_result t recipient.name recipient_result
  in
  match Player.equal user recipient with
  | true -> new_game_state
  | false -> add_private_result t user.name user_result
;;

let apply_gamblers_potion t (action : Action.t) (item_effect : Item_effect.t)
  =
  (* get a float between 0 and 1 to represent some probability,
     considered a success if this float is less than or equal
     to the success chance, otherwise we remove the health *)
  let recipient = Map.find_exn t.players action.recipient in
  let recipient_name = recipient.name
  and previous_health = recipient.health
  and uniform_chance = Random.float 1.0 in
  let recipient_with_updated_health, new_game_state =
    match Float.( <= ) uniform_chance item_effect.chance_of_adding with
    | true ->
      let health_to_add = item_effect.add_health in
      (* intentionally allow health to overflow in case someone was 
         stabbed and used a bandage within the same round. We currently 
         do not give any specific priority to who uses an item first 
         so we only consider net changes in health. The health will be 
         capped at 100 when computing the final round results, i.e. who 
         died in the round and other information *)
      let new_health = previous_health + health_to_add in
      ( { recipient with health = new_health }
      , add_gamblers_potion_message t action health_to_add )
    | false ->
      let health_to_subtract = item_effect.remove_health in
      (* same comment as above, we allow health to go below 0 but will 
         cap at 0 when displaying results *)
      let new_health = previous_health - health_to_subtract in
      ( { recipient with health = new_health }
      , add_gamblers_potion_message t action health_to_subtract )
  in
  { new_game_state with
    players =
      Map.set
        new_game_state.players
        ~key:recipient_name
        ~data:recipient_with_updated_health
  }
;;

let add_successful_item_use_message
      t
      (action : Action.t)
      (health_change : int)
  =
  let user = Map.find_exn t.players action.user
  and recipient = Map.find_exn t.players action.recipient
  and item = action.item_used in
  let (user_result : Round_result.t), (recipient_result : Round_result.t) =
    match 0 < health_change with
    (* health was added *)
    | true ->
      ( { player_in_question = recipient.name
        ; message =
            [%string "gained %{health_change#Int}HP from your %{item#Item}"]
        }
      , { player_in_question = user.name
        ; message =
            [%string "gave you %{health_change#Int}HP by using %{item#Item}"]
        } )
    (* health was removed*)
    | false ->
      let health_change = abs health_change in
      ( { player_in_question = recipient.name
        ; message =
            [%string "lost %{health_change#Int}HP from your %{item#Item}"]
        }
      , { player_in_question = user.name
        ; message =
            [%string
              "removed %{health_change#Int}HP from you using %{item#Item}"]
        } )
  in
  let new_game_state =
    add_private_result t recipient.name recipient_result
  in
  match Player.equal user recipient with
  | true -> new_game_state
  | false -> add_private_result new_game_state user.name user_result
;;

let add_failed_item_use_message t (action : Action.t) =
  let user = Map.find_exn t.players action.user
  and recipient = Map.find_exn t.players action.recipient
  and item = action.item_used in
  let (user_result : Round_result.t), (recipient_result : Round_result.t) =
    ( { player_in_question = recipient.name
      ; message = [%string "failed to use %{item#Item} on %{recipient.name}"]
      }
    , { player_in_question = user.name
      ; message = [%string "failed to use %{item#Item} on you"]
      } )
  in
  let new_game_state =
    add_private_result t recipient.name recipient_result
  in
  match Player.equal user recipient with
  | true -> new_game_state
  | false -> add_private_result new_game_state user.name user_result
;;

let apply_item_effect t (action : Action.t) (item_effect : Item_effect.t) =
  (* as a convention, uniform_chance must be less than or equal to
     the chance of add/removing health to be considered successful*)
  let uniform_chance = Random.float 1.0 in
  let success_chance, health_change =
    match Float.( <= ) item_effect.chance_of_removing 0.0 with
    | true -> item_effect.chance_of_adding, item_effect.add_health
    | false -> item_effect.chance_of_removing, item_effect.remove_health * -1
  in
  match Float.( <= ) uniform_chance success_chance with
  | true ->
    let recipient = Map.find_exn t.players action.recipient in
    let new_health = health_change + recipient.health in
    let recipient = { recipient with health = new_health } in
    let new_game_state =
      { t with
        players = Map.set t.players ~key:recipient.name ~data:recipient
      }
    in
    add_successful_item_use_message new_game_state action health_change
  | false -> add_failed_item_use_message t action
;;

let apply_actions_taken t =
  List.fold
    t.actions_taken_in_round
    ~init:t
    ~f:(fun acc_state action_taken ->
      match action_taken.item_used with
      | Item.Observer -> add_observer_message acc_state action_taken
      | Item_interception ->
        add_item_interception_message acc_state action_taken
      | Gamblers_potion item_effect ->
        apply_gamblers_potion acc_state action_taken item_effect
      | Medical_kit item_effect
      | Poisonous_dart item_effect
      | Pocket_knife item_effect ->
        apply_item_effect acc_state action_taken item_effect)
;;

let compile_all_results t = ()
