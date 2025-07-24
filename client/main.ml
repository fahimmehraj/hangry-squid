open! Core
open Bonsai_web
open Bonsai.Let_syntax

(* An example of a small Bonsai component for creating counter
   widgets ("+" and "-" buttons allow you to increment and decrement a
   number). The state of each counter is housed in a shared [Map]. Adding
   a new counter is as simple as adding a value to the map. *)

let header = {%html|
  <h1>Hangry Games</h1>
|}

let player_in_waiting_room name avatar_url =
  {%html|
  <div class="player">
      <img class="avatar" src=%{avatar_url#String} />
      <h2>%{name#String}</h2>
  </div>
|}
;;

let component graph =
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
  let%arr state and inject in
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
  Vdom.Node.div (add_button :: counters)
;;

let players = List.init 8 ~f:(fun i -> Printf.sprintf "Player %d" i)

let app graph =
  let player_view =
    Vdom.Node.div
      ~attrs:[ [%css {|
    display: flex;
    padding: 8px;
  |}] ]
      (List.map players ~f:(fun player ->
         player_in_waiting_room
           player
           "https://upload.wikimedia.org/wikipedia/en/c/c1/Seong_Gi-hun_season_1.png"))
  in
  let component = component graph in
  let%arr component in
  Vdom.Node.div [ header; player_view; component ]
;;

let () = Bonsai_web.Start.start app
