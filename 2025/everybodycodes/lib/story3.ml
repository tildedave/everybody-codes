open Base

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

type color = Red | Green | Blue [@@deriving sexp, compare]
type shine = Matte | Shiny [@@deriving sexp, compare]

let dominant_color l =
  match l with
  | [ _; r; g; b; _ ] ->
      if r > g && r > b then Some Red
      else if g > r && g > b then Some Green
      else if b > g && b > r then Some Blue
      else None
  | _ -> failwith "invalid"

let shine l =
  match l with
  | [ _; _; _; _; s ] ->
      if s <= 30 then Some Matte else if s >= 33 then Some Shiny else None
  | _ -> failwith "invalid"

let group l =
  Option.bind (dominant_color l) ~f:(fun c ->
      Option.bind (shine l) ~f:(fun s -> Some (c, s)))

let%test_unit "group" =
  [%test_eq: (color * shine) option list]
    [
      None;
      Some (Red, Matte);
      Some (Red, Matte);
      Some (Green, Matte);
      Some (Blue, Shiny);
      Some (Blue, Matte);
      Some (Blue, Matte);
      None;
      Some (Red, Shiny);
      Some (Blue, Shiny);
      Some (Blue, Shiny);
      Some (Blue, Matte);
      None;
      Some (Red, Matte);
      Some (Red, Matte);
      Some (Green, Shiny);
    ]
    (List.map
       ~f:(fun l -> group (parse l))
       [
         "15437:rRrrRR gGGGGG BBBBBB sSSSSS";
         "94682:RrRrrR gGGggG bBBBBB ssSSSs";
         "56513:RRRrrr ggGGgG bbbBbb ssSsSS";
         "76346:rRRrrR GGgggg bbbBBB ssssSs";
         "87569:rrRRrR gGGGGg BbbbbB SssSss";
         "44191:rrrrrr gGgGGG bBBbbB sSssSS";
         "49176:rRRrRr GggggG BbBbbb sSSssS";
         "85071:RRrrrr GgGGgg BBbbbb SSsSss";
         "44303:rRRrrR gGggGg bBbBBB SsSSSs";
         "94978:rrRrRR ggGggG BBbBBb SSSSSS";
         "26325:rrRRrr gGGGgg BBbBbb SssssS";
         "43463:rrrrRR gGgGgg bBBbBB sSssSs";
         "15059:RRrrrR GGgggG bbBBbb sSSsSS";
         "85004:RRRrrR GgGgGG bbbBBB sSssss";
         "56121:RRrRrr gGgGgg BbbbBB sSsSSs";
         "80219:rRRrRR GGGggg BBbbbb SssSSs";
       ])

let quest1part3 l =
  l
  |> List.map ~f:(fun l -> parse l)
  |> List.map ~f:(fun l -> (List.hd_exn l, group l))
  |> List.filter_map ~f:(fun (i, o) ->
         match o with None -> None | Some p -> Some (i, p))
  |> List.sort_and_group ~compare:(fun (_, (c1, s1)) (_, (c2, s2)) ->
         match compare_color c1 c2 with 0 -> compare_shine s1 s2 | c -> c)
  |> List.max_elt ~compare:(fun l1 l2 ->
         compare (List.length l1) (List.length l2))
  |> Option.value_exn
  |> List.fold ~init:zero ~f:(fun n (i, _) -> i + n)

let%test_unit "quest1part3" =
  [%test_eq: int]
    (quest1part3
       [
         "15437:rRrrRR gGGGGG BBBBBB sSSSSS";
         "94682:RrRrrR gGGggG bBBBBB ssSSSs";
         "56513:RRRrrr ggGGgG bbbBbb ssSsSS";
         "76346:rRRrrR GGgggg bbbBBB ssssSs";
         "87569:rrRRrR gGGGGg BbbbbB SssSss";
         "44191:rrrrrr gGgGGG bBBbbB sSssSS";
         "49176:rRRrRr GggggG BbBbbb sSSssS";
         "85071:RRrrrr GgGGgg BBbbbb SSsSss";
         "44303:rRRrrR gGggGg bBbBBB SsSSSs";
         "94978:rrRrRR ggGggG BBbBBb SSSSSS";
         "26325:rrRRrr gGGGgg BBbBbb SssssS";
         "43463:rrrrRR gGgGgg bBBbBB sSssSs";
         "15059:RRrrrR GGgggG bbBBbb sSSsSS";
         "85004:RRRrrR GgGgGG bbbBBB sSssss";
         "56121:RRrRrr gGgGgg BbbbBB sSsSSs";
         "80219:rRRrRR GGGggg BBbbbb SssSSs";
       ])
    292320
