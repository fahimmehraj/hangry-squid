open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let item item =
  let image_url = Item.image item in
  let name = Item.to_string item in
  let description = Item.description item in
  {%html|
    <div class="item">
      <img src=%{image_url#String}/>
      <p>%{name#String}</p>
      <p>%{description#String}</p>
    </div>
|}
;;

