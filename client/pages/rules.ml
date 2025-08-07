open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var
(* open Bonsai.Let_syntax *)

let rules =
  {%html|
  <div>
    <ol>
      <li>Each player starts with 100 health points shown with the health bar</li>
      <li>Game consists of 10 rounds</li>
      <li>In each round you have 15 seconds to select an item, 60 seconds to discuss with others, and 15 seconds to use any items</li>
      <li>Items are one time use only and not selecting items will result in a pass and all items transfer to the next round</li>
      <li>Survive as many rounds as you can to take a larger share of the prize money</li>
    </ol>
  </div>
|}
;;

let items =
  [ Item.medical_kit ; Item.observer ; Item.poisonous_dart ; Item.gamblers_potion ; Item.pocket_knife ; Item.item_blocker ]
  |> List.map ~f:Components.item
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
