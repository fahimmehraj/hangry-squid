module Url_var = Bonsai_web_ui_url_var

val avatar_urls : string list

val url_by_name : string -> string

val item
  :  ?on_click:
       (Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> unit Ui_effect.t)
  -> ?selected:bool
  -> Hangry_squid.Item.t
  -> Bonsai_web.Vdom.Node.t

val healthbar : string -> int -> Bonsai_web.Vdom.Node.t

val header
  :  Hangry_squid.Player.t
  -> phase:Hangry_squid.Game_phase.t
  -> seconds_left:int
  -> Bonsai_web.Vdom.Node.t
