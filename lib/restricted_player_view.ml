open! Core

type t =
  { health : int
  ; is_alive : bool
  ; name : string
  }
[@@deriving sexp, bin_io]

let equal t1 t2 = String.equal t1.name t2.name

let of_player ({ health; is_alive; name; _ } : Player.t) : t =
  { health; is_alive; name }
;;
