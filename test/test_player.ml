open! Core
open Hangry_squid

let%expect_test "test_new_player" =
  let new_player = Player.new_player "jeff" in
  print_s [%sexp (new_player : Player.t)];
  [%expect {|((health 100) (inventory ()) (is_alive true) (name jeff))|}]
;;
