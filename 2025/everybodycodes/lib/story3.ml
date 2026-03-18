open Base

(* type Color = Red | Green | Blue *)

let to_binary s =
  String.fold s ~init:0 ~f:(fun n ch ->
      (n * 2)
      +
      match ch with
      | 'R' -> 1
      | 'G' -> 1
      | 'B' -> 1
      | 'r' -> 0
      | 'g' -> 0
      | 'b' -> 0
      | _ -> assert false)

let%test_unit "to_binary" = [%test_eq: int] (to_binary "rrrRRr") 6

let part1_parse l =
  match String.split ~on:':' l with
  | [ id_str; color_str ] ->
      Int.of_string id_str
      :: List.map ~f:to_binary (String.split ~on:' ' color_str)
  | _ -> assert false

let%test_unit "part1_parse" =
  [%test_eq: int list]
    (part1_parse "2456:rrrrrr ggGgGG bbbbBB")
    [ 2456; 0; 11; 3 ]

let quest1part1 l =
  l |> List.map ~f:part1_parse
  |> List.filter ~f:(fun l ->
         match l with [ _; r; g; b ] -> g > r && g > b | _ -> assert false)
  |> List.map ~f:List.hd_exn
  |> List.fold ~f:( + ) ~init:zero

let%test_unit "quest1part1" =
  [%test_eq: int]
    (quest1part1
       [
         "2456:rrrrrr ggGgGG bbbbBB";
         "7689:rrRrrr ggGggg bbbBBB";
         "3145:rrRrRr gggGgg bbbbBB";
         "6710:rrrRRr ggGGGg bbBBbB";
       ])
    9166
