open! Core
open Hangry_squid

let empty = Game_state.create_empty_game ()

let%expect_test "test_empty_game_state" =
  print_s [%sexp (empty : Game_state.t)];
  [%expect {|
    ((current_round 0) (players ()) (actions_taken_in_round ()) 
     (public_results ()) (private_results ()))|}]
;;

(* let%expect_test "test_add_action" = 
  let action = {

  }
  print_s [%sexp (Game_state.add_action empty )] *)
