open! Core
open Hangry_squid

let%expect_test "test_new_player" =
  let new_player = Player.new_player "jeff" in
  print_s [%sexp (new_player : Player.t)];
  [%expect {|((health 100) (inventory ()) (is_alive true) (name jeff))|}]
;;

let%expect_test "test_equal_players" =
  let new_player = Player.new_player "jeff" in
  print_s [%sexp (Player.equal new_player new_player : bool)];
  [%expect {|true|}]
;;

let%expect_test "test_equal_players" =
  let new_player = Player.new_player "jeff" in
  let new_player_2 = Player.new_player "jeff" in
  print_s [%sexp (Player.equal new_player new_player_2 : bool)];
  [%expect {|true|}]
;;

let%expect_test "test_unequal_players" =
  let new_player = Player.new_player "jeff" in
  let new_player_2 = Player.new_player "Jeff" in
  print_s [%sexp (Player.equal new_player new_player_2 : bool)];
  [%expect {|false|}]
;;
