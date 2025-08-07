open! Core
open! Hangry_squid
open Bonsai_web
open Bonsai.Let_syntax

let join_status_state_machine (local_ graph) =
  let dispatcher = Utils.dispatcher graph in
  Bonsai.state_machine_with_input
    ~default_model:(Entering_name ("", None) : Landing_state.t)
    ~apply_action:(fun ctx input (model : Landing_state.t) action ->
      match input with
      | Active dispatcher ->
        (match action with
         | `Ack_join accepted_name -> Accepted_name accepted_name
         | `Failed_join error ->
           Entering_name (Landing_state.name model, Some error)
         | `Update_name new_name -> Entering_name (new_name, None)
         | `Try_to_join_game ->
           if String.equal "" (Landing_state.name model)
           then model
           else (
             let player_name_query =
               Rpcs.Client_message.Query.New_player
                 (Landing_state.name model)
             in
             let my_new_effect =
               let%bind.Effect result = dispatcher player_name_query in
               match result with
               | Error error ->
                 Bonsai.Apply_action_context.inject
                   ctx
                   (`Failed_join (Error.to_string_hum error))
               | Ok resp ->
                 (match resp with
                  | Ok _ ->
                    Bonsai.Apply_action_context.inject
                      ctx
                      (`Ack_join (Landing_state.name model))
                  | Error error_message ->
                    Bonsai.Apply_action_context.inject
                      ctx
                      (`Failed_join error_message))
             in
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               my_new_effect;
             model))
      | Inactive -> model)
    dispatcher
    graph
;;

let body update_join_game_state error =
  let%arr update_join_game_state
  and error
  and rules_section = Rules.body () in
  let join_game_button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            update_join_game_state `Try_to_join_game)
        ]
      [ Vdom.Node.text "Join Game" ]
  in
  let select_name_input =
    Vdom.Node.input
      ~attrs:
        [ Vdom.Attr.placeholder "Enter name"
        ; Vdom.Attr.type_ "text"
        ; Vdom.Attr.class_ "name-input"
        ; Vdom.Attr.on_input (fun _ current_name ->
            update_join_game_state (`Update_name current_name))
        ; Vdom.Attr.on_keydown (fun event ->
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event event with
            | Enter -> update_join_game_state `Try_to_join_game
            | _ -> Effect.all_unit [])
        ]
      ()
  in
  let error_message =
    let error_text =
      match error with None -> "" | Some error_message -> error_message
    in
    Vdom.Node.div
      ~attrs:[ [%css {|
        color: red;
        |}] ]
      [ Vdom.Node.text error_text ]
  in
  {%html|
    <div class="header">
      <div class="header-row">
        <h1>Hangry Games</h1>
        <h3>Enter your name to join the waiting room</h3>
        <div class="w-48">
          <div class="m-1">
            %{select_name_input}
          </div>
          <div class="m-1">
            %{join_game_button}
          </div>
        </div>
        <div class="m-4">
          %{error_message}
        </div>
        <div class="mx-auto items-center">
          %{rules_section}
        </div>
      </div>
    </div>
  |}
;;
