open! Core

type t =
  | Observer
  | Item_interception
  | Medical_kit of Item_effect.t
  | Poisonous_dart of Item_effect.t
  | Pocket_knife of Item_effect.t
  | Gamblers_potion of Item_effect.t
[@@deriving sexp]

let observer = Observer
let item_interception = Item_interception

let medical_kit =
  Medical_kit (Item_effect.create ~add_health:25 ~chance_of_adding:1.0 ())
;;

let poisonous_dart =
  Poisonous_dart
    (Item_effect.create ~remove_health:75 ~chance_of_removing:0.75 ())
;;

let pocket_knife =
  Pocket_knife
    (Item_effect.create ~remove_health:30 ~chance_of_removing:1.0 ())
;;

let gamblers_potion =
  Gamblers_potion
    (Item_effect.create
       ~add_health:60
       ~chance_of_adding:0.6
       ~remove_health:40
       ~chance_of_removing:0.4
       ())
;;

let to_string t : string =
  match t with
  | Observer -> "Observer"
  | Item_interception -> "Item Interception"
  | Medical_kit _ -> "Medical Kit"
  | Poisonous_dart _ -> "Poisonous Dart"
  | Pocket_knife _ -> "Pocket Knife"
  | Gamblers_potion _ -> "Gambler's Potion"
;;

let equal item1 item2 : bool =
  match item1, item2 with
  | Observer, Observer
  | Item_interception, Item_interception
  | Medical_kit _, Medical_kit _
  | Poisonous_dart _, Poisonous_dart _
  | Pocket_knife _, Pocket_knife _
  | Gamblers_potion _, Gamblers_potion _ ->
    true
  | _, _ -> false
;;

let get_two_random_items_no_duplicates () : t * t =
  let possible_items =
    [ observer
    ; item_interception
    ; medical_kit
    ; poisonous_dart
    ; pocket_knife
    ; gamblers_potion
    ]
  in
  let first_item = List.random_element_exn possible_items in
  let remaining_items_to_choose_from =
    List.filter possible_items ~f:(fun other_item ->
      not (equal other_item first_item))
  in
  let second_item = List.random_element_exn remaining_items_to_choose_from in
  first_item, second_item
;;
