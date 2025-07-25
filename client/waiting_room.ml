open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var

let header =
  {%html|
  <div>
    <h1>Hangry Games</h1>
    <p>Waiting for games to start</p>
  </div>
|}
;;

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
    width: 200px;
  height: 200px;
  border-radius: 50%; /* Makes the image circular */
  object-fit: cover; /* Ensures the image covers the area without distortion */
  display: block; /* Removes extra space below the image if it's inline */
  |}]
  in
  Vdom.Node.div
    ~attrs:[ container_styles ]
    [ Vdom.Node.img ~attrs:[ Vdom.Attr.src avatar_url; avatar_styles ] ()
    ; Vdom.Node.h2 [ Vdom.Node.text name ]
    ]
;;

let component (local_ graph) route =
  let state, inject =
    Bonsai.state_machine
      ~default_model:Int.Map.empty
      ~apply_action:(fun _ctx model -> function
        | `New_counter -> Map.add_exn model ~key:(Map.length model) ~data:0
        | `Incr_by (counter_id, diff) ->
          Map.update model counter_id ~f:(function
            | None -> diff
            | Some x -> x + diff))
      graph
  in
  let%arr state and inject and route in
  let button text action =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject action) ]
      [ Vdom.Node.text text ]
  in
  let add_button = button "add" `New_counter in
  let for_each (i, c) =
    Vdom.Node.div
      [ button "-1" (`Incr_by (i, -1))
      ; Vdom.Node.textf "%d" c
      ; button "+1" (`Incr_by (i, 1))
      ]
  in
  let counters = state |> Map.to_alist |> List.map ~f:for_each in
  let debug =
    Vdom.Node.p [ Vdom.Node.text (Sexp.to_string (Page.sexp_of_t route)) ]
  in
  Vdom.Node.div (add_button :: debug :: counters)
;;

let players = List.init 8 ~f:(fun i -> Printf.sprintf "Player %d" (i + 1))

let page route (local_ graph) =
  let player_view =
    Vdom.Node.div
      ~attrs:
        [ [%css
            {|
    display: flex;
    padding: 8px;
    gap: 20px;
    flex-wrap: wrap;
  |}]
        ]
      (List.map players ~f:(fun player ->
         player_in_waiting_room
           player
           "https://upload.wikimedia.org/wikipedia/en/c/c1/Seong_Gi-hun_season_1.png"))
  in
  let component = component graph route in
  let%arr component in
  Vdom.Node.div [ header; player_view; component ]
;;
