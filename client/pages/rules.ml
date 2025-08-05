open! Core
open Bonsai_web
module Url_var = Bonsai_web_ui_url_var
(* open Bonsai.Let_syntax *)

type item =
  { name : string
  ; description : string
  ; image_url : string
  }

let rules =
  {%html|
  <div>
    <ol>
      <li>Each player starts with 100 health points shown with the health bar</li>
      <li>Game consists of 10 rounds</li>
      <li>In each round you have 15 seconds to select an item, 60 seconds to discuss with others, and 15 seconds to use any items</li>
      <li>Items are one time use only and not selecting items will result in a pass and all items transfer to the next round</li>
      <li>Survive as many rounds as you can</li>
      <li>Lie, back stab, help, and do whatever it takes to take a larger share of the prize money</li>
    </ol>
  </div>
|}
;;

let item { name; description; image_url } =
  {%html|
    <div class="item">
      <img src=%{image_url#String}/>
      <p class="bold-text">%{name#String}</p>
      <p>%{description#String}</p>
    </div>
|}
;;

let items =
  [ { name = "Medical Kit"
    ; description = "Restores 25 HP. Can be used on anyone"
    ; image_url = "/assets/medical_kit.png"
    }
  ; { name = "Observer"
    ; description =
        "Allows you to see into any one person's inventory and action taken"
    ; image_url = "/assets/observer.png"
    }
  ; { name = "Poisonous Dart"
    ; description = "Deals 75 damage but has a 25% chance of missing"
    ; image_url = "/assets/poison_arrow.png"
    }
  ; { name = "Gamblers Potion"
    ; description = "60% to gain 60 HP\n40% to lose 40 HP"
    ; image_url = "/assets/gamblers_potion.png"
    }
  ; { name = "Pocket Knife"
    ; description = "Deals 30 damage"
    ; image_url = "/assets/pocket_knife.png"
    }
  ; { name = "Item Interception"
    ; description =
        "Block one player from receiving an item in the next round"
    ; image_url = "/assets/item_blocker.png"
    }
  ]
  |> List.map ~f:item
  |> Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "items" ] ]
;;

let content = Vdom.Node.div [ rules; items ]

let body () = Bonsai.return (
  {%html|
    <div class="flex-column mx-auto items-center">
      %{content}
    </div>
  |}
)
