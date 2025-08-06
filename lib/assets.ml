open! Core

let avatar_urls =
  [ "../client/assets/player1.jpg"
  ; "../client/assets/player2.webp"
  ; "../client/assets/player3.webp"
  ; "../client/assets/player4.jpg"
  ; "../client/assets/player5.webp"
  ; "../client/assets/player6.png"
  ; "../client/assets/player7.jpeg"
  ; "../client/assets/player8.png"
  ]
;;

let url_by_name name =
  let index = (String.hash name) % (List.length avatar_urls) in
  List.nth_exn avatar_urls index
;;
