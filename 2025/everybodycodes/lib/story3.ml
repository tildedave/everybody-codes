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
      | 'S' -> 1
      | 'r' -> 0
      | 'g' -> 0
      | 'b' -> 0
      | 's' -> 0
      | _ -> assert false)

let%test_unit "to_binary" = [%test_eq: int] (to_binary "rrrRRr") 6

let parse l =
  match String.split ~on:':' l with
  | [ id_str; color_str ] ->
      Int.of_string id_str
      :: List.map ~f:to_binary (String.split ~on:' ' color_str)
  | _ -> assert false

let%test_unit "parse" =
  [%test_eq: int list] (parse "2456:rrrrrr ggGgGG bbbbBB") [ 2456; 0; 11; 3 ]

let quest1part1 l =
  l |> List.map ~f:parse
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

let color_sum l =
  List.fold ~f:( + ) ~init:zero @@ List.tl_exn (List.drop_last_exn l)

let%test_unit "color_sum" =
  [%test_eq: int] (color_sum [ 2456; 0; 11; 3; 21 ]) 14

let quest1part2 l =
  l |> List.map ~f:parse
  |> List.sort_and_group ~compare:(fun l1 l2 ->
         compare (List.last_exn l2) (List.last_exn l1))
  |> List.hd_exn
  |> List.sort_and_group ~compare:(fun l1 l2 ->
         compare (color_sum l1) (color_sum l2))
  |> List.hd_exn |> List.hd_exn |> List.hd_exn

let%test_unit "quest1part2" =
  [%test_eq: int]
    (quest1part2
       [
         "2456:rrrrrr ggGgGG bbbbBB sSsSsS";
         "7689:rrRrrr ggGggg bbbBBB ssSSss";
         "3145:rrRrRr gggGgg bbbbBB sSsSsS";
         "6710:rrrRRr ggGGGg bbBBbB ssSSss";
       ])
    2456
