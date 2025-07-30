open! Core
open Hangry_squid

let%expect_test "test_random_items_no_duplicates" =
  Random.init 100;
  print_s
    [%sexp (Item.get_two_random_items_no_duplicates () : Item.t * Item.t)];
  [%expect
    {|
    ((Pocket_knife
      ((add_health 0) (chance_of_adding 0) (remove_health 30)
       (chance_of_removing 1)))
     Item_blocker)  
  |}]
;;

let%expect_test "test_never_get_duplicates" =
  let rec loop n =
    match n with
    | 0 -> ()
    | _ ->
      let item1, item2 = Item.get_two_random_items_no_duplicates () in
      print_s [%sexp (Item.equal item1 item2 : bool)];
      [%expect {|false|}];
      loop (n - 1)
  in
  loop 65535
;;
