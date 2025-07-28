open! Core

type t =
  | Observer
  | Item_blocker
  | Medical_kit of Item_effect.t
  | Poisonous_dart of Item_effect.t
  | Pocket_knife of Item_effect.t
  | Gamblers_potion of Item_effect.t
[@@deriving sexp]

let observer = Observer
let item_blocker = Item_blocker

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
  | Item_blocker -> "Item Blocker"
  | Medical_kit _ -> "Medical Kit"
  | Poisonous_dart _ -> "Poisonous Dart"
  | Pocket_knife _ -> "Pocket Knife"
  | Gamblers_potion _ -> "Gambler's Potion"
;;

let description = function
  | Observer -> "Allows you to see into any one person's inventory"
  | Medical_kit { add_health; _ } ->
    [%string "Restores %{add_health#Int} HP"]
  | Item_blocker ->
    "Block one player from receiving an item in the next round"
  | Poisonous_dart { remove_health; chance_of_removing; _ } ->
    [%string
      "Deals %{remove_health#Int} damage but has a \
       %{chance_of_removing#Float} chance of missing"]
  | Pocket_knife { remove_health; _ } ->
    [%string "Deals %{remove_health#Int} damage"]
  | Gamblers_potion
      { add_health; remove_health; chance_of_adding; chance_of_removing } ->
    [%string
      "%{chance_of_adding#Float}% to gain %{add_health#Int} HP\n\
       %{chance_of_removing#Float}% to gain %{remove_health#Int} HP"]
;;

let image = function
| Observer -> "observer.png"
| Item_blocker -> "item_blocker.png"
| Medical_kit _ -> "medical_kit.png"
| Poisonous_dart _ -> "poison_arrow.png"
| Pocket_knife _ -> "pocket_knife.png"
| Gamblers_potion _ -> "gamblers_potion.png"

let equal item1 item2 : bool =
  match item1, item2 with
  | Observer, Observer
  | Item_blocker, Item_blocker
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
    ; item_blocker
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
