open! Core

type t = {
  players : Player.t String.Map.t;
      (* map from player name to a player type, names should be unique*)
  current_round : int;
  actions_taken_in_round : Action.t list;
  public_results : Result.t list;
  private_results : Result.t String.Map.t
}

let create_empty_game () =
  {
    players = Map.empty (module String);
    current_round = 0;
    actions_taken_in_round = [];
    public_results = [];
    private_results = Map.empty (module String)
  }

let apply_actions_taken
let compile_all_results
