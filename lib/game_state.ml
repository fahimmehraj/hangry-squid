open! Core

type t = 
{ players : Player.t String.Map.t (* map from player name to a player type, names should be unique*)
; current_round : int
; 
}
