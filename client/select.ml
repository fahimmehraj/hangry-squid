open! Core
open Bonsai_web
module Url_var = Bonsai_web_ui_url_var

let header =
  {%html|
  <div class="header">
    <h1>Hangry Games</h1>
    <p>Item Selection Phase</p>
    <p>15 seconds left in the current phase</p>
  </div>
|}
;;

