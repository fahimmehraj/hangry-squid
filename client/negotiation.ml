open! Core
open Bonsai_web
open Hangry_squid
module Url_var = Bonsai_web_ui_url_var
(* open Bonsai.Let_syntax *)

type party =
  | Me
  | Other

let header =
  {%html|
  <div class="header">
    <h1>Hangry Games</h1>
    <p>Negotiation Phase</p>
    <p>60 seconds left in the current phase</p>
  </div>
|}
;;

let items =
  [ Item.pocket_knife; Item.pocket_knife; Item.pocket_knife ; Item.pocket_knife ; Item.pocket_knife ; Item.pocket_knife ; Item.pocket_knife]
  |> List.map ~f:Components.item
  |> Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "inventory" ] ]
;;

let msg_bubble content ~who =
  let bubble_color = match who with Me -> "#000000" | Other -> "#E1E1E1" in
  let text_color = match who with Me -> "#ffffff" | Other -> "#000000" in
  let bubble_alignment =
    match who with Me -> "flex-end" | Other -> "flex-start"
  in
  let bubble_styles =
    [%css
      {|
    background-color: %{bubble_color};
    padding: 8px 24px;
    color: %{text_color}; 
    flex-direction: column;
    align-self: %{bubble_alignment};
    align-items: center;
    border-radius: 12px;
    font-family: "Inter";
  |}]
  in
  Vdom.Node.div
    ~attrs:[ bubble_styles ]
    [ Vdom.Node.p [ Vdom.Node.text content ] ]
;;

let players =
  List.init 8 ~f:(fun i -> Printf.sprintf "Player %d" (i + 1))
  |> List.map ~f:(fun name ->
    { Restricted_player_view.name; health = 75; is_alive = true })
  |> List.filter ~f:(fun { is_alive; _ } -> is_alive)
  |> List.map ~f:(fun { name; health; _ } ->
    let green_end = Int.to_string (health - 5) ^ "%" in
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
        [ Vdom.Node.p [ Vdom.Node.text red_start ] ]
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
      ])
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
      border-right: 1px solid #000000;
      display: flex;
      flex-direction: column;
      gap: 16px;
      overflow-y: auto;
      overflow-x: hidden;
    |}]
         ]
;;

let messages =
  [ msg_bubble "Yo" ~who:Other
  ; msg_bubble "I have a big knife" ~who:Other
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ; msg_bubble "Fr?" ~who:Me
  ]
  |> Vdom.Node.div
       ~attrs:
         [ [%css
             {|
    display: flex;
    flex-direction: column;
    flex-grow: 1;
    gap: 2px;
    overflow: auto;
  |}]
         ]
;;

let reply_box =
  Vdom.Node.input
    ~attrs:
      [ [%css
          {|
    position: sticky;
    bottom: 0;
    width: 75%;
    box-sizing: border-box;
    margin-left: 4px;
    padding: 3px 3px;
    font-size: 16px;
  |}]
      ; Vdom.Attr.placeholder "Reply"
      ]
    ()
;;

let messages_window =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    flex-direction: column;
    flex-grow: 1;
    overflow: auto;

  |}]
      ]
    [ messages; reply_box ]
;;

let chat_window =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: flex;
    border: 2px solid #000000;
    gap: 8px;
    overflow-y: auto;

  |}]
      ]
    [ players; messages_window ]
;;

let chat_window_with_header =
  Vdom.Node.div ~attrs:[[%css {|
    display: flex;
    flex-direction: column;
    overflow-y: auto;
  |}]]
  [ Vdom.Node.h2 [Vdom.Node.text "Chat"] ; chat_window]

let inventory_window_with_header =
  Vdom.Node.div ~attrs:[[%css {|
    display: flex;
    flex-direction: column;
    overflow-y: auto;
  |}]]
  [ Vdom.Node.h2 [Vdom.Node.text "Inventory"] ; items]

let content =
  Vdom.Node.div
    ~attrs:
      [ [%css
          {|
    display: grid;
    grid-template-columns: 2fr 1fr;
    gap: 1rem;
    flex-grow: 1;
    overflow-y: auto;
    height: 100%;
    margin: 16px;
  |}]
      ]
    [ chat_window_with_header; inventory_window_with_header ]
;;

let next_phase_button url_var =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.on_click (fun _ ->
          Url_var.set_effect url_var Page.Action)
      ]
    [ Vdom.Node.text "next phase" ]
;;

let body url_var =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:
         [ [%css {| height: 100%; display: flex; flex-direction: column; |}]
         ]
       [ header; next_phase_button url_var; content ])
;;
