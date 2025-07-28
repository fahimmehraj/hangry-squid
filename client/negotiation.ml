open! Core
open Bonsai_web
module Url_var = Bonsai_web_ui_url_var
(* open Bonsai.Let_syntax *)

type item =
  { name : string
  ; description : string
  ; image_url : string
  }

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

let item { name; description; image_url } =
  {%html|
    <div class="item">
      <img src=%{image_url#String}/>
      <p>%{name#String}</p>
      <p>%{description#String}</p>
    </div>
|}
;;

let items =
  [ { name = "Pocket Knife"
    ; description = "(25 damage)"
    ; image_url = "/assets/pocket_knife.png"
    }
  ; { name = "Item Interception"
    ; description = "Block next item for player"
    ; image_url = "/assets/item_blocker.png"
    }
  ; { name = "Item Interception"
    ; description = "Block next item for player"
    ; image_url = "/assets/item_blocker.png"
    }
  ; { name = "Item Interception"
    ; description = "Block next item for player"
    ; image_url = "/assets/item_blocker.png"
    }
  ; { name = "Item Interception"
    ; description = "Block next item for player"
    ; image_url = "/assets/item_blocker.png"
    }
  ]
  |> List.map ~f:item
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
  |}]
  in
  Vdom.Node.div
    ~attrs:[ bubble_styles ]
    [ Vdom.Node.p [ Vdom.Node.text content ] ]
;;

type player =
  { name : string
  ; health : int
  }

let players =
  List.init 8 ~f:(fun i -> Printf.sprintf "Player %d" (i + 1))
  |> List.map ~f:(fun name -> { name; health = 75 })
  |> List.map ~f:(fun { name; health } ->
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
      [ Vdom.Node.p [ Vdom.Node.text name ]; healthbar ])
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
    bottom: 4px;
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
    position: relative;
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
    [ chat_window; items ]
;;

let body () =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:
         [ [%css {| height: 100%; display: flex; flex-direction: column; |}]
         ]
       [ header; content ])
;;

(* /* Frame 2610405 */

/* Auto layout */
display: flex;
flex-direction: row;
justify-content: center;
align-items: center;
padding: 6px 60px;
gap: 10px;

position: absolute;
width: 298px;
height: 42px;
left: 27px;
top: 73px; *)
