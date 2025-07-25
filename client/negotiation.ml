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
    ; image_url = "/client/assets/pocket_knife.png"
    }
  ; { name = "Item Interception"
    ; description = "Block next item for player"
    ; image_url = "/client/assets/item_blocker.png"
    }
  ]
  |> List.map ~f:item
  |> Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "inventory" ] ]
;;

let msg_bubble content ~who =
  let bubble_color = match who with Me -> "#000000" | Other -> "#E1E1E1" in
  let bubble_alignment =
    match who with Me -> "flex-end" | Other -> "flex-start"
  in
  let bubble_styles =
    [%css
      {|
    background-color: %{bubble_color};
    padding: 2px 24px;
    flex-direction: column;
    align-self: %{bubble_alignment};
    align-items: center;
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
  |> List.map ~f:(fun name -> { name; health = 100 })
  |> List.map ~f:(fun { name; health } ->
    let health = Int.to_string health in
    let healthbar_styles =
      [%css
        {|
    width: 100px; /* Example fixed width, adjust as needed */
  height: 25px; /* Example fixed height, adjust as needed */
  background: linear-gradient(
    to right, /* Or to bottom, to top, to left, or an angle like 45deg */
    #ff0000 0%,    /* First color starts at 0% */
    #ff0000 %{health - 1}%,   /* First color ends at 23% */
    #0000ff 26%,   /* Second color starts at 26% */
    #0000ff 100%   /* Second color ends at 100% */
  );
  /* Optional: Add a border or box-shadow for better visibility */
  border: 1px solid #ccc;
    |}]
    in
    {%html|
    <div>
      <p>%{name#String}</p>
      <div />
    </div>
  |})
;;

let body () = Bonsai.return (Vdom.Node.div [ header ])
