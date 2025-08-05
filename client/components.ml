open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let avatar_urls =
  [ "../client/assets/player1.jpg"
  ; "../client/assets/player2.webp"
  ; "../client/assets/player3.webp"
  ; "../client/assets/player4.jpg"
  ; "../client/assets/player5.webp"
  ; "../client/assets/player6.png"
  ; "../client/assets/player7.jpeg"
  ; "../client/assets/player8.png"
  ]
;;

let url_by_name name = 
  let index = (String.hash name) % (List.length avatar_urls) in
  List.nth_exn avatar_urls index
;;

let item ?(on_click = fun _ -> Effect.all_unit []) ?(selected = false) item =
  let image_url = "/assets/" ^ Item.image item in
  let name = Item.to_string item in
  let description = Item.description item in
  let selected_container_style =
    match selected with
    | true ->
      Css_gen.border ~width:(`Px 1) ~color:(`Hex "#4BB4FF") ~style:`Solid ()
    | false -> Css_gen.empty
  in
  let selected_name_style =
    match selected with
    | true -> Css_gen.color (`Hex "#4BB4FF")
    | false -> Css_gen.empty
  in

  {%html|
    <div class="item" style=%{selected_container_style} on_click=%{on_click}>
      <img src=%{image_url#String}/>
      <p style=%{selected_name_style}>%{name#String}</p>
      <p style=%{selected_name_style}>%{description#String}</p>
    </div>
|}
;;

let healthbar name health =
  let green_end = Int.to_string (health - 1) ^ "%" in
  let red_start = Int.to_string health ^ "%" in
  let healthbar_styles =
    [%css
      {|
          display: flex;
          flex-direction: row;
          justify-content: center;
          align-items: center;
          padding: 2px 2px;
          width: 150px;
          height: 20px;
          background: linear-gradient(90deg, #9DD593 %{green_end}, #F7A0A0 %{red_start});
          border: 1px solid #ccc;
    |}]
  in
  let healthbar =
    Vdom.Node.div
      ~attrs:[ healthbar_styles ]
      [ Vdom.Node.p [ Vdom.Node.text (Int.to_string health ^ " HP") ] ]
  in
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
        padding: 4px;
        display: flex;
        flex-direction: column;
        gap: 4px;
      |}]
      ]
    [ Vdom.Node.p
        ~attrs:[ [%css {| font-family: "Inter"; |}] ]
        [ Vdom.Node.text name ]
    ; healthbar
    ]
;;

let header ({ name; health; _ } : Player.t) ~phase ~seconds_left =
  let phase_name = Game_phase.to_string phase in
  let beneath_text =
    match phase with
    | Game_phase.Waiting_room -> ""
    | _ -> Int.to_string seconds_left ^ " seconds left in the current phase"
  in
  {%html|
  <div class="header">
    <div class="header-row">
      <h1 class="less-bottom-margin">Hangry Games</h1>
      <p class="subtitle">%{phase_name#String}</p>
      <p class="mb-6">%{beneath_text#String}</p>
      <div class="last-item">
        %{healthbar name health}
      </div>
    </div>
  </div>
|}
;;
