open! Core
open Hangry_squid

let bob = Player.new_player "bob"
let jeff = Player.new_player "jeff"
let empty = Game_state.create_empty_game ()
let one_player = Game_state.add_player empty bob
let two_player = Game_state.add_player one_player jeff
let pocket_knife_action : Action.t =
    { user = bob; recipient = jeff; item_used = Item.pocket_knife }
let two_player_knife_used = Game_state.add_action two_player pocket_knife_action
let two_player_knife_used_results = Game_state.apply_actions_taken two_player_knife_used
let knife_self_game = Game_state.add_action one_player { user = bob ; recipient = bob ; item_used = Item.pocket_knife }
let knife_self = Game_state.apply_actions_taken knife_self_game

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
      (((user ((health 100) (inventory ()) (is_alive true) (name bob)))
        (recipient ((health 100) (inventory ()) (is_alive true) (name jeff)))
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_results ()) (private_results ())) |}]
;;

let%expect_test "test_apply_actions_taken" = 
  print_s [%sexp (two_player_knife_used_results : Game_state.t)];
  [%expect {|
    ((current_round 0)
     (players
      ((bob ((health 100) (inventory ()) (is_alive true) (name bob)))
       (jeff ((health 70) (inventory ()) (is_alive true) (name jeff)))))
     (actions_taken_in_round
      (((user ((health 100) (inventory ()) (is_alive true) (name bob)))
        (recipient ((health 100) (inventory ()) (is_alive true) (name jeff)))
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_results ())
     (private_results
      ((bob
        (((player_in_question
           ((health 100) (inventory ()) (is_alive true) (name jeff)))
          (message "lost 30HP from your Pocket Knife"))))
       (jeff
        (((player_in_question
           ((health 100) (inventory ()) (is_alive true) (name bob)))
          (message "removed 30HP from you using Pocket Knife"))))))) |}]
;;

let%expect_test "test_knife_self" =
  print_s [%sexp (knife_self : Game_state.t)];
  [%expect {|
    ((current_round 0)
     (players ((bob ((health 70) (inventory ()) (is_alive true) (name bob)))))
     (actions_taken_in_round
      (((user ((health 100) (inventory ()) (is_alive true) (name bob)))
        (recipient ((health 100) (inventory ()) (is_alive true) (name bob)))
        (item_used
         (Pocket_knife
          ((add_health 0) (chance_of_adding 0) (remove_health 30)
           (chance_of_removing 1)))))))
     (public_results ())
     (private_results
      ((bob
        (((player_in_question
           ((health 100) (inventory ()) (is_alive true) (name bob)))
          (message "removed 30HP from you using Pocket Knife"))))))) |}]
;;
