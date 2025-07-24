open! Core
open Hangry_squid

let bob = Player.new_player "bob"
let jeff = Player.new_player "jeff"
let empty = Game_state.create_empty_game ()
let one_player = Game_state.add_player empty bob
let two_player = Game_state.add_player one_player jeff

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

let%expect_test "test_empty_game_state" =
  print_s [%sexp (empty : Game_state.t)];
  [%expect
    {|
    ((current_round 0) (players ()) (actions_taken_in_round ()) 
     (public_results ()) (private_results ()))|}]
;;

let%expect_test "test_add_player" =
  print_s [%sexp (one_player : Game_state.t)];
  [%expect
    {|
    ((current_round 0) 
     (players ((bob ((health 100) (inventory ()) (is_alive true) (name bob))))) 
     (actions_taken_in_round ()) (public_results ()) (private_results ())) |}]
;;

let%expect_test "test_add_duplicate" =
  Printexc.record_backtrace false;
  print_s [%sexp (one_player : Game_state.t)];
  [%expect
    {|
    ((current_round 0) 
     (players ((bob ((health 100) (inventory ()) (is_alive true) (name bob))))) 
     (actions_taken_in_round ()) (public_results ()) (private_results ())) |}];
  let _ = Game_state.add_player one_player (Player.new_player "bob") in
  [%expect.unreachable]
[@@expect.uncaught_exn {|(Failure "Name already taken")|}]
;;

let%expect_test "test_add_action" =
  print_s [%sexp (two_player_knife_used : Game_state.t)];
  [%expect
    {|
      ((current_round 0)
       (players
        ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
         (jeff ((health 100) (inventory ()) (is_alive true) (name jeff)))))
       (actions_taken_in_round
        (((user bob) (recipient jeff)
          (item_used
           (Pocket_knife
            ((add_health 0) (chance_of_adding 0) (remove_health 30)
             (chance_of_removing 1)))))))
       (public_results ()) (private_results ())) |}]
;;

let%expect_test "test_apply_actions_taken" =
  print_s [%sexp (two_player_knife_used_results : Game_state.t)];
  [%expect
    {|
      ((current_round 0)
       (players
        ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
         (jeff ((health 70) (inventory ()) (is_alive true) (name jeff)))))
       (actions_taken_in_round
        (((user bob) (recipient jeff)
          (item_used
           (Pocket_knife
            ((add_health 0) (chance_of_adding 0) (remove_health 30)
             (chance_of_removing 1)))))))
       (public_results ())
       (private_results
        ((bob
          (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
         (jeff
          (((player_in_question bob)
            (message "removed 30HP from you using Pocket Knife"))))))) |}]
;;

let%expect_test "test_knife_self" =
  print_s [%sexp (knife_self : Game_state.t)];
  [%expect
    {|
      ((current_round 0)
       (players ((bob ((health 70) (inventory ()) (is_alive true) (name bob)))))
       (actions_taken_in_round
        (((user bob) (recipient bob)
          (item_used
           (Pocket_knife
            ((add_health 0) (chance_of_adding 0) (remove_health 30)
             (chance_of_removing 1)))))))
       (public_results ())
       (private_results
        ((bob
          (((player_in_question bob)
            (message "removed 30HP from you using Pocket Knife"))))))) |}]
;;

let%expect_test "test_observe_self" =
  print_s [%sexp (observe_self : Game_state.t)];
  [%expect
    {|
      ((current_round 0)
       (players ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))))
       (actions_taken_in_round (((user bob) (recipient bob) (item_used Observer))))
       (public_results ())
       (private_results
        ((bob
          (((player_in_question bob)
            (message  "Inventory Observed: \
                     \nActions Observed: Observer"))))))) |}]
;;

let%expect_test "test_observe_other" =
  print_s [%sexp (observe_jeff : Game_state.t)];
  [%expect
    {|
      ((current_round 0)
       (players
        ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
         (jeff ((health 70) (inventory ()) (is_alive true) (name jeff)))))
       (actions_taken_in_round
        (((user jeff) (recipient bob) (item_used Observer))
         ((user bob) (recipient jeff)
          (item_used
           (Pocket_knife
            ((add_health 0) (chance_of_adding 0) (remove_health 30)
             (chance_of_removing 1)))))))
       (public_results ())
       (private_results
        ((bob
          (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
         (jeff
          (((player_in_question bob)
            (message "removed 30HP from you using Pocket Knife"))
           ((player_in_question bob)
            (message  "Inventory Observed: \
                     \nActions Observed: Pocket Knife"))))))) |}]
;;

let%expect_test "test_heal_after_knife" = 
  print_s [%sexp (heal_after_pocket_knife : Game_state.t)];
  [%expect {|
    ((current_round 0)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 95) (inventory ()) (is_alive true) (name jeff)))))
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
     (public_results ())
     (private_results
      ((bob
        (((player_in_question jeff) (message "lost 30HP from your Pocket Knife"))))
       (jeff
        (((player_in_question bob)
          (message "removed 30HP from you using Pocket Knife"))
         ((player_in_question jeff)
          (message "gave you 25HP by using Medical Kit"))))))) |}]
;;
