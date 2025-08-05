open! Core

type t =
  { current_round : int
  ; current_phase : Game_phase.t
  ; players : Player.t String.Map.t
  ; ready_players : string list
  ; actions_taken_in_round : Action.t list
  ; public_messages : Message.t list
  ; private_messages : Message.t list String.Map.t String.Map.t
  ; public_results : Round_result.t list
  ; private_results : Round_result.t list String.Map.t
  ; item_choices_by_user : (Item.t * Item.t) option String.Map.t
  ; round_start : Time_ns.t
  }
[@@deriving sexp]

let create_empty_game () =
  { current_round = 0
  ; current_phase = Waiting_room
  ; players = Map.empty (module String)
  ; ready_players = []
  ; actions_taken_in_round = []
  ; public_messages = []
  ; private_messages = String.Map.empty
  ; public_results = []
  ; private_results = Map.empty (module String)
  ; item_choices_by_user = Map.empty (module String)
  ; round_start = Time_ns.now ()
  }
;;

let get_private_messages t name =
  match Map.find t.private_messages name with
  | None -> String.Map.empty
  | Some map -> map
;;

let get_client_state_from_name (t : t) (name : string) : Client_state.t =
  let current_round = t.current_round in
  let current_phase = t.current_phase in
  let players =
    Map.data t.players
    |> List.map ~f:Restricted_player_view.of_player
  in
  let ready_players = t.ready_players in
  let public_messages = t.public_messages in
  let my_messages = get_private_messages t name in
  let public_results = t.public_results in
  let my_results =
    match Map.find t.private_results name with
    | None -> []
    | Some results -> results
  in
  let item_choices =
    match Map.find t.item_choices_by_user name with
    | None -> None
    | Some choice -> choice
  in
  let round_start = t.round_start in
  let me =
    match Map.find t.players name with
    | None -> failwith "Player not found"
    | Some player -> player
  in
  { current_round
  ; current_phase
  ; players
  ; ready_players
  ; public_messages
  ; my_messages
  ; public_results
  ; my_results
  ; item_choices
  ; round_start
  ; me
  }
;;

let name_taken t name =
  match Map.find t.players name with None -> false | Some _ -> true
;;

let ready_player t (query : Rpcs.Client_message.Ready_status_change.t) =
  match query.is_ready with
  | false ->
    let new_ready_players =
      List.filter t.ready_players ~f:(fun player_name ->
        not (String.equal player_name query.name))
    in
    { t with ready_players = new_ready_players }
  | true ->
    if not (List.mem t.ready_players query.name ~equal:String.equal)
    then (
      let new_ready_players = query.name :: t.ready_players in
      { t with ready_players = new_ready_players })
    else t
;;

let add_item_to_inventory t (query : Rpcs.Client_message.Item_selection.t) =
  match Map.find t.players query.name with
  | None -> t
  | Some player ->
    let new_inventory = query.item :: player.inventory in
    let new_player = { player with inventory = new_inventory } in
    { t with players = Map.set t.players ~key:query.name ~data:new_player }
;;

let update_message_map map key message =
  let new_messages =
    match Map.find map key with
    | None -> [ message ]
    | Some exchanged_messages -> message :: exchanged_messages
  in
  Map.set map ~key ~data:new_messages
;;

let update_private_messages t ~map ~key =
  let new_private_messages = Map.set t.private_messages ~key ~data:map in
  { t with private_messages = new_private_messages }
;;

let add_public_message t (message : Message.t) =
  { t with public_messages = message :: t.public_messages }
;;

let add_message t (message : Message.t) =
  let sender = message.sender in
  let recipient = message.recipient in
  let recipient =
    match recipient with
    | None ->
      failwith
        "Cannot add a message between two players: missing recipient. Only \
         should be omitted if sending a public message. Should not be \
         reached assuming we check for public messages beforehand."
    | Some recipient -> recipient
  in
  let sender_message_map = get_private_messages t sender in
  let recipient_message_map = get_private_messages t recipient in
  (* check if the sender has ever sent messages to the recipient before *)
  let sender's_new_message_map =
    update_message_map sender_message_map recipient message
  in
  (* and vice versa *)
  let recipient's_new_message_map =
    update_message_map recipient_message_map sender message
  in
  update_private_messages t ~map:sender's_new_message_map ~key:sender
  |> update_private_messages ~map:recipient's_new_message_map ~key:recipient
;;

let add_player t (player : Player.t) =
  match Map.find t.players player.name with
  | None ->
    { t with players = Map.set t.players ~key:player.name ~data:player }
  | Some _ -> failwith "Name already taken"
;;

let add_action t (action : Action.t) =
  let item_used = action.item_used in
  let player_name = action.user in
  let player = Map.find_exn t.players player_name in
  let rec remove_first l =
    match l with
    | [] -> []
    | item :: rest ->
      if Item.equal item item_used then rest else item :: remove_first rest
  in
  let new_inventory = remove_first player.inventory in
  let new_player = { player with inventory = new_inventory } in
  let new_player_map = Map.set t.players ~key:player_name ~data:new_player in
  { t with
    actions_taken_in_round = action :: t.actions_taken_in_round
  ; players = new_player_map
  }
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

let add_item_blocker_message t (action : Action.t) =
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
  let cleared_private_results =
    { t with private_results = String.Map.empty ; public_results = [] }
  in
  List.fold
    t.actions_taken_in_round
    ~init:cleared_private_results
    ~f:(fun acc_state action_taken ->
      match action_taken.item_used with
      | Item.Observer -> add_observer_message acc_state action_taken
      | Item_blocker -> add_item_blocker_message acc_state action_taken
      | Gamblers_potion item_effect ->
        apply_gamblers_potion acc_state action_taken item_effect
      | Medical_kit item_effect
      | Poisonous_dart item_effect
      | Pocket_knife item_effect ->
        apply_item_effect acc_state action_taken item_effect)
;;

let compile_all_elimination_results t =
  Map.fold t.players ~init:t ~f:(fun ~key ~data acc_state ->
    let player_name = key in
    let player = data in
    let health = player.health in
    match health > 100 with
    (* cap the player's health at 100 in case multiple healing items were used*)
    | true ->
      let player_with_health_updated = { player with health = 100 } in
      { acc_state with
        players =
          Map.set
            acc_state.players
            ~key:player_name
            ~data:player_with_health_updated
      }
    | false ->
      (match health <= 0 with
       (* round all health below 0 to 0 so that no one's health bar shows -30 HP*)
       | true ->
         let player_with_health_updated =
           { player with health = 0; is_alive = false }
         in
         let new_game_state =
           { acc_state with
             players =
               Map.set
                 acc_state.players
                 ~key:player_name
                 ~data:player_with_health_updated
           }
         in
         let elimination_result : Round_result.t =
           { player_in_question = player_name
           ; message =
               [%string
                 "was eliminated in round \
                  %{new_game_state.current_round#Int}"]
           }
         in
         { new_game_state with
           public_results =
             elimination_result :: new_game_state.public_results
         }
       | false -> acc_state))
;;

let players_left t =
  Map.fold t.players ~init:0 ~f:(fun ~key ~data acc ->
    ignore key;
    if data.is_alive then acc + 1 else acc)
;;
