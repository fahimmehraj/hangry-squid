open! Core
open Hangry_squid

let bob = Player.new_player "bob"
let jeff = Player.new_player "jeff"
let empty = Game_state.create_empty_game ()
let one_player = Game_state.add_player empty bob
let two_player = Game_state.add_player one_player jeff
let round_start = Time_ns.max_value_representable

let pocket_knife_action : Action.t =
  { user = "bob"; recipient = "jeff"; item_used = Item.pocket_knife }
;;

let two_player_knife_used =
  Game_state.add_action two_player pocket_knife_action
;;

let two_player_knife_used_results =
  Game_state.apply_actions_taken two_player_knife_used
;;

let knife_self_game =
  Game_state.add_action
    one_player
    { user = "bob"; recipient = "bob"; item_used = Item.pocket_knife }
;;

let knife_self = Game_state.apply_actions_taken knife_self_game

let observe_self =
  Game_state.add_action
    one_player
    { user = "bob"; recipient = "bob"; item_used = Item.observer }
  |> Game_state.apply_actions_taken
;;

let observe_jeff =
  Game_state.add_action
    two_player_knife_used
    { user = "jeff"; recipient = "bob"; item_used = Item.observer }
  |> Game_state.apply_actions_taken
;;

let heal_after_pocket_knife =
  Game_state.add_action
    two_player_knife_used
    { user = "jeff"; recipient = "jeff"; item_used = Item.medical_kit }
  |> Game_state.apply_actions_taken
;;

let gamblers_potion seed =
  Random.init seed;
  let after_knife =
    Game_state.add_action
      two_player
      { user = "bob"; recipient = "bob"; item_used = Item.pocket_knife }
  in
  Game_state.add_action
    after_knife
    { user = "jeff"; recipient = "bob"; item_used = Item.gamblers_potion }
  |> Game_state.apply_actions_taken
;;

let message : Message.t =
  { sender = "bob"
  ; recipient = Some "jeff"
  ; contents = "yo gurt"
  ; timestamp = round_start
  }
;;

let message_sent = Game_state.add_message two_player message
let sexp_of_t t = Game_state.sexp_of_t { t with round_start }

let%expect_test "test_empty_game_state" =
  print_s [%sexp (sexp_of_t empty : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room) (players ())
     (ready_players ()) (actions_taken_in_round ()) (public_messages ())
     (private_messages ()) (public_results ()) (private_results ())
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_add_player" =
  print_s [%sexp (sexp_of_t one_player : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))))
     (ready_players ()) (actions_taken_in_round ()) (public_messages ())
     (private_messages ()) (public_results ()) (private_results ())
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_add_duplicate" =
  Printexc.record_backtrace false;
  print_s [%sexp (sexp_of_t one_player : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))))
     (ready_players ()) (actions_taken_in_round ()) (public_messages ())
     (private_messages ()) (public_results ()) (private_results ())
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))
  |}];
  let _ = Game_state.add_player one_player (Player.new_player "bob") in
  [%expect.unreachable]
[@@expect.uncaught_exn {|(Failure "Name already taken")|}]
;;

let%expect_test "test_add_action" =
  print_s [%sexp (sexp_of_t two_player_knife_used : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 100) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results ()) (item_choices_by_user ())
     (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_apply_actions_taken" =
  print_s [%sexp (sexp_of_t two_player_knife_used_results : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 70) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
       (jeff
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_knife_self" =
  print_s [%sexp (sexp_of_t knife_self : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players ((bob ((health 70) (inventory ()) (is_alive true) (name bob)))))
     (ready_players ())
     (actions_taken_in_round
      (((user bob) (recipient bob)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_observe_self" =
  print_s [%sexp (sexp_of_t observe_self : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))))
     (ready_players ())
     (actions_taken_in_round (((user bob) (recipient bob) (item_used Observer))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question bob)
          (message  "Inventory Observed: \
                   \nActions Observed: Observer"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_observe_other" =
  print_s [%sexp (sexp_of_t observe_jeff : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 70) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user jeff) (recipient bob) (item_used Observer))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
       (jeff
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question bob)
          (message  "Inventory Observed: \
                   \nActions Observed: Pocket Knife"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_heal_after_knife" =
  print_s [%sexp (sexp_of_t heal_after_pocket_knife : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 95) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user jeff) (recipient jeff)
        (item_used
         (Medical_kit
          ((add_health 25) (chance_of_adding 1) (remove_health 0)
           (chance_of_removing 0)))))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
       (jeff
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question jeff)
          (message "gave you 25HP by using Medical Kit"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_gamblers_potion_add_health" =
  print_s [%sexp (sexp_of_t (gamblers_potion 100) : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 130) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 100) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user jeff) (recipient bob)
        (item_used
         (Gamblers_potion
          ((add_health 60) (chance_of_adding 0.6) (remove_health 40)
           (chance_of_removing 0.4)))))
       ((user bob) (recipient bob)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))))
       (jeff (((player_in_question bob) (message "gained 60HP because of you"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))
  |}]
;;

let%expect_test "test_gamblers_potion_remove_health" =
  print_s [%sexp (sexp_of_t (gamblers_potion 77) : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 130) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 100) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user jeff) (recipient bob)
        (item_used
         (Gamblers_potion
          ((add_health 60) (chance_of_adding 0.6) (remove_health 40)
           (chance_of_removing 0.4)))))
       ((user bob) (recipient bob)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results
      ((bob
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))))
       (jeff (((player_in_question bob) (message "gained 60HP because of you"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))
  |}]
;;

let%expect_test "test_compile_all_results" =
  print_s
    [%sexp
      (sexp_of_t
         (Game_state.compile_all_elimination_results two_player_knife_used)
       : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 100) (inventory ()) (is_alive true) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ()) (public_results ())
     (private_results ()) (item_choices_by_user ())
     (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;

let%expect_test "test_compile_all_results_multiple knives" =
  let g =
    Fn.apply_n_times
      ~n:5
      (fun game_state ->
        Game_state.add_action game_state pocket_knife_action)
      two_player_knife_used
    |> Game_state.apply_actions_taken
  in
  print_s
    [%sexp
      (sexp_of_t (Game_state.compile_all_elimination_results g) : Sexp.t)];
  [%expect
    {|
    ((current_round 0) (current_phase Waiting_room)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 0) (inventory ()) (is_alive false) (name jeff)))))
     (ready_players ())
     (actions_taken_in_round
      (((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))
       ((user bob) (recipient jeff)
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_messages ()) (private_messages ())
     (public_results
      (((player_in_question jeff) (message "was eliminated in round 0"))))
     (private_results
      ((bob
        (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))
         ((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))
         ((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))
         ((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))
         ((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))
         ((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
       (jeff
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))))))
     (item_choices_by_user ()) (round_start (2116-02-20 23:53:38.427387903Z)))  
  |}]
;;
