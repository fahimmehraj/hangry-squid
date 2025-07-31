open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var

let player_in_waiting_room name avatar_url =
  let container_styles =
    [%css
      {|
    display: flex;
    flex-direction: column;
    align-items: center;
  |}]
  in
  let avatar_styles =
    [%css
      {|
    width: 100px;
  height: 100px;
  border-radius: 50%; 
  object-fit: cover; 
  display: block; 
  |}]
  in
  Vdom.Node.div
    ~attrs:[ container_styles ]
    [ Vdom.Node.img ~attrs:[ Vdom.Attr.src avatar_url; avatar_styles ] ()
    ; Vdom.Node.p [ Vdom.Node.text name ]
    ]
;;

let result_component (result : Round_result.t) =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: row;
    align-items: center;
    gap: 24px;
  |}]
      ]
    [ player_in_waiting_room
        result.player_in_question
        "https://upload.wikimedia.org/wikipedia/en/c/c1/Seong_Gi-hun_season_1.png"
    ; Vdom.Node.p [ Vdom.Node.text result.message ]
    ]
;;

let private_results =
  [ { Round_result.player_in_question = "Player 7"
    ; message = "Stabbed you for 25 health points"
    }
  ; { player_in_question = "Player 3"
    ; message = "Stabbed you for 25 health points"
    }
  ; { player_in_question = "Player 6"; message = "Failed to stab you" }
  ; { player_in_question = "Player 7"
    ; message = "was stabbed by you for 25 damage"
    }
  ]
  |> List.map ~f:result_component
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
  display: flex;
  flex-direction: column;
  gap: 8px;
|}]
         ]
;;

let public_results =
  [ { Round_result.player_in_question = "Player 2"; message = "has died" }
  ; { player_in_question = "Player 4"; message = "has died" }
  ]
  |> List.map ~f:result_component
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
  display: flex;
  flex-direction: column;
  gap: 8px;
|}]
         ]
;;

let left_section =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Private Results" ]; private_results ]
;;

let right_section =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    height: 100%;
  |}]
      ]
    [ Vdom.Node.h2 [ Vdom.Node.text "Public Results" ]; public_results ]
;;

let content =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    justify-content: space-around;
    margin: 8px;
    gap: 8px;
    flex-grow: 1;
  |}]
      ]
    [ left_section; right_section ]
;;

let body (client_state : Client_state.t Bonsai.t) (local_ graph) =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:
         [ [%css
             {|
  height: 100%;
  display: flex;
  flex-direction: column;
|}]
         ]
       [ content ])
;;
