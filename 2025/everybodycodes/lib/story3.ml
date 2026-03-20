[@@@ocaml.warning "-32"]

open Base
open Util

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

let positions lines =
  let grid = to_grid lines in
  ( Option.value_exn (grid_find ~f:(equal_char '@') grid),
    Set.of_list (module IntPair_Comparator)
    @@ grid_find_all ~f:(equal_char '#') grid )

type direction = Up | Right | Left | Down [@@deriving sexp, compare, show]

let dir_seq = Sequence.cycle_list_exn [ Up; Right; Down; Left ]

let walk (x, y) dir =
  match dir with
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let quest2part1 lines =
  let start, finish_set = positions lines in
  let finish = List.hd_exn @@ Set.to_list finish_set in
  Sequence.unfold
    ~init:(start, Set.empty (module IntPair_Comparator), dir_seq)
    ~f:(fun (curr, seen, dirs) ->
      if equal_tuple curr finish then None
      else
        let next_dir, rest_dirs =
          (Sequence.hd_exn dirs, Sequence.tl_eagerly_exn dirs)
        in
        let next = walk curr next_dir in
        if Set.mem seen next then Some (0, (curr, seen, rest_dirs))
        else Some (1, (next, Set.add seen curr, rest_dirs)))
  |> Sequence.fold ~init:0 ~f:( + )

let%test_unit "quest2part1" =
  [%test_eq: int]
    (quest2part1
       [
         ".......";
         ".......";
         ".......";
         ".#.@...";
         ".......";
         ".......";
         ".......";
       ])
    12

let neighbors coord = List.map ~f:(walk coord) [ Up; Down; Left; Right ]
let is_surrounded s c = List.for_all (neighbors c) ~f:(Set.mem s)

let%test_unit "neighbors" =
  [%test_eq: (int * int) list]
    [ (0, -1); (0, 1); (-1, 0); (1, 0) ]
    (neighbors (0, 0))

let%test_unit "is_surrounded" =
  [%test_eq: bool] true
    (is_surrounded
       (Set.of_list
          (module IntPair_Comparator)
          [ (0, -1); (0, 1); (-1, 0); (1, 0) ])
       (0, 0))

type 'a sound = {
  coords : (int * int, 'a) Set.t;
  bounds : (int * int) * (int * int);
}

let set_bounds sound =
  let xs, ys =
    (List.map ~f:fst (Set.to_list sound), List.map ~f:snd (Set.to_list sound))
  in
  let xmin = List.fold xs ~init:Int.max_value ~f:min in
  let xmax = List.fold xs ~init:Int.min_value ~f:max in
  let ymin = List.fold ys ~init:Int.max_value ~f:min in
  let ymax = List.fold ys ~init:Int.min_value ~f:max in
  ((xmin, ymin), (xmax, ymax))

let is_oob ((xmin, ymin), (xmax, ymax)) (x, y) =
  x < xmin || y < ymin || x > xmax || y > ymax

let add_sound sound (x, y) =
  let (xmin, ymin), (xmax, ymax) = sound.bounds in
  {
    coords = Set.add sound.coords (x, y);
    bounds = ((min xmin x, min ymin y), (max xmax x, max ymax y));
  }

let flood_fill sound start =
  let rec helper queue visited frontier =
    match queue with
    | [] -> Some visited
    | curr :: rest_queue -> (
        let n = neighbors curr in
        match List.find n ~f:(is_oob sound.bounds) with
        | None ->
            let next_queue, next_frontier =
              List.fold n ~init:(rest_queue, frontier) ~f:(fun (q, f) c ->
                  if Set.mem f c then (q, f) else (c :: q, Set.add f c))
            in
            helper next_queue (Set.add visited curr) next_frontier
        | _ -> None)
  in
  helper [ start ] (Set.empty (module IntPair_Comparator)) sound.coords

let%test_unit "flood_fill (filled example)" =
  [%test_eq: (int * int) list option]
    (Some [ (-1, 1); (0, 1); (1, 1) ])
    (Option.map ~f:Set.to_list
       (flood_fill
          {
            coords =
              Set.of_list
                (module IntPair_Comparator)
                [
                  (-2, 0);
                  (-1, 0);
                  (0, 0);
                  (1, 0);
                  (2, 0);
                  (-2, 2);
                  (-1, 2);
                  (0, 2);
                  (1, 2);
                  (2, 2);
                  (-2, 1);
                  (2, 1);
                ];
            bounds = ((-3, -3), (3, 3));
          }
          (0, 1)))

let%test_unit "flood_fill (unfilled example)" =
  [%test_eq: (int * int) list option] None
    (Option.map ~f:Set.to_list
       (flood_fill
          {
            coords =
              Set.of_list
                (module IntPair_Comparator)
                [
                  (-2, 0);
                  (0, 0);
                  (1, 0);
                  (2, 0);
                  (-2, 2);
                  (-1, 2);
                  (0, 2);
                  (1, 2);
                  (2, 2);
                  (-2, 1);
                  (2, 1);
                ];
            bounds = ((-3, -3), (3, 3));
          }
          (0, 1)))

let fill_spot sound coord =
  List.fold (neighbors coord) ~init:(add_sound sound coord) ~f:(fun s c ->
      if Set.mem s.coords c then s
      else
        match flood_fill s c with
        | Some coords -> Set.fold ~init:s ~f:add_sound coords
        | None -> s)

let show_sound ~curr ~finish_set sound =
  let result = Buffer.create 0 in
  let (xmin, ymin), (xmax, ymax) = sound.bounds in
  for y = ymin - 3 to ymax + 3 do
    for x = xmin - 3 to xmax + 3 do
      Buffer.add_char result
        (if equal_tuple curr (x, y) then '@'
         else if Set.mem finish_set (x, y) then '#'
         else if Set.mem sound.coords (x, y) then '+'
         else '.')
    done;
    Buffer.add_char result '\n'
  done;
  Buffer.contents result

let can_escape s c = Option.is_none (flood_fill s c)

let propagate_sound directions lines =
  let start, vocal_bones_original = positions lines in
  let sound =
    let s = Set.add vocal_bones_original start in
    { coords = s; bounds = set_bounds s }
  in
  Sequence.unfold ~init:(start, sound, vocal_bones_original, directions)
    ~f:(fun (curr, sound, vocal_bones, dirs) ->
      (* Stdio.printf "%s\n"
         (show_sound sound ~curr ~finish_set:vocal_bones_original); *)
      if Set.is_empty vocal_bones then
        (* Stdio.printf "%s\n" (show_sound sound ~curr ~finish_set); *)
        None
      else
        let next_dir, rest_dirs =
          (Sequence.hd_exn dirs, Sequence.tl_eagerly_exn dirs)
        in
        let next = walk curr next_dir in
        if Set.mem sound.coords next then
          Some (0, (curr, sound, vocal_bones, rest_dirs))
        else
          let next_sound = fill_spot sound next in
          Some
            ( 1,
              ( next,
                next_sound,
                Set.filter vocal_bones ~f:(can_escape next_sound),
                rest_dirs ) ))
  |> Sequence.fold ~init:0 ~f:( + )

let quest2part2 = propagate_sound dir_seq

let quest2part3 =
  propagate_sound
  @@ Sequence.cycle_list_exn
       [ Up; Up; Up; Right; Right; Right; Down; Down; Down; Left; Left; Left ]

let%test_unit "quest2part2" =
  [%test_eq: int]
    (quest2part2
       [
         ".......";
         ".......";
         ".......";
         ".#.@...";
         ".......";
         ".......";
         ".......";
       ])
    47

let%test_unit "quest2part3" =
  [%test_eq: int]
    (quest2part3
       [
         "#..#.......#...";
         "...#...........";
         "...#...........";
         "#######........";
         "...#....#######";
         "...#...@...#...";
         "...#.......#...";
         "...........#...";
         "...........#...";
         "#..........#...";
         "##......#######";
       ])
    239

let parse_shape s =
  match String.split ~on:' ' s with
  | [ color; shape ] -> (color, shape)
  | _ -> failwith "impossible"

let node_re =
  Re.Perl.re
    {|id=(\d+), plug=([A-Z]+ [A-Z]+), leftSocket=([A-Z]+ [A-Z]+), rightSocket=([A-Z]+ [A-Z]+), data=(.*)|}
  |> Re.compile

type pluggable = string * string [@@deriving equal, show, compare, sexp]

type node = {
  id : int;
  plug : pluggable;
  left_socket : pluggable;
  right_socket : pluggable;
  data : string;
}
[@@deriving show, compare, sexp]

let parse_node s =
  match Re.exec_opt node_re s with
  | Some groups ->
      {
        id = Int.of_string @@ Re.Group.get groups 1;
        plug = parse_shape @@ Re.Group.get groups 2;
        left_socket = parse_shape @@ Re.Group.get groups 3;
        right_socket = parse_shape @@ Re.Group.get groups 4;
        data = Re.Group.get groups 5;
      }
  | None -> failwith "invalid input"

let%test_unit "parse_node" =
  [%test_eq: node]
    {
      id = 1;
      plug = ("BLUE", "HEXAGON");
      left_socket = ("GREEN", "CIRCLE");
      right_socket = ("BLUE", "PENTAGON");
      data = "?";
    }
    (parse_node
       "id=1, plug=BLUE HEXAGON, leftSocket=GREEN CIRCLE, rightSocket=BLUE \
        PENTAGON, data=?")

type tree = Node of node * tree option * tree option
[@@deriving show, compare, sexp]

let empty_tree node = Node (node, None, None)

type bond_type = Strong | Weak

let equal_pluggable_bond (c1, s1) (c2, s2) ~bond_type =
  match bond_type with
  | Strong -> equal_pluggable (c1, s1) (c2, s2)
  | Weak -> equal_string c1 c2 || equal_string s1 s2

let rec insert_node node (Node (curr, left, right)) ~bond_type =
  match
    match left with
    | None ->
        if equal_pluggable_bond node.plug curr.left_socket ~bond_type then
          Some (empty_tree node)
        else None
    | Some left' -> insert_node node left' ~bond_type
  with
  | Some new_left -> Some (Node (curr, Some new_left, right))
  | None -> (
      match
        match right with
        | None ->
            if equal_pluggable_bond node.plug curr.right_socket ~bond_type then
              Some (empty_tree node)
            else None
        | Some right' -> insert_node node right' ~bond_type
      with
      | Some new_right -> Some (Node (curr, left, Some new_right))
      | None -> None)

let build_tree lines ~bond_type =
  let nodes = List.map ~f:parse_node lines in
  List.fold
    ~init:(empty_tree @@ List.hd_exn nodes)
    ~f:(fun tree node -> Option.value_exn @@ insert_node node tree ~bond_type)
    (List.tl_exn nodes)

let integers = Sequence.unfold ~init:1 ~f:(fun n -> Some (n, n + 1))

let rec traverse (Node (curr, left, right)) =
  Sequence.concat
  @@ Sequence.of_list
       [
         (match left with
         | None -> Sequence.empty
         | Some left' -> traverse left');
         Sequence.singleton curr;
         (match right with
         | None -> Sequence.empty
         | Some right' -> traverse right');
       ]

let quest3part1 lines =
  let tree = build_tree lines ~bond_type:Strong in
  Sequence.fold ~init:0
    ~f:(fun n (node, i) -> n + (node.id * i))
    (Sequence.zip (traverse tree) integers)

let%test_unit "quest3part1" =
  [%test_eq: int] 43
    (quest3part1
       [
         "id=1, plug=BLUE HEXAGON, leftSocket=GREEN CIRCLE, rightSocket=BLUE \
          PENTAGON, data=?";
         "id=2, plug=GREEN CIRCLE, leftSocket=BLUE HEXAGON, rightSocket=BLUE \
          CIRCLE, data=?";
         "id=3, plug=BLUE PENTAGON, leftSocket=BLUE CIRCLE, rightSocket=BLUE \
          CIRCLE, data=?";
         "id=4, plug=BLUE CIRCLE, leftSocket=RED HEXAGON, rightSocket=BLUE \
          HEXAGON, data=?";
         "id=5, plug=RED HEXAGON, leftSocket=GREEN CIRCLE, rightSocket=RED \
          HEXAGON, data=?";
       ])

let quest3part2 lines =
  let tree = build_tree lines ~bond_type:Weak in
  Sequence.fold ~init:0
    ~f:(fun n (node, i) -> n + (node.id * i))
    (Sequence.zip (traverse tree) integers)

let%test_unit "quest3part2" =
  [%test_eq: int] 50
    (quest3part2
       [
         "id=1, plug=RED TRIANGLE, leftSocket=RED TRIANGLE, rightSocket=RED \
          TRIANGLE, data=?";
         "id=2, plug=GREEN TRIANGLE, leftSocket=BLUE CIRCLE, rightSocket=GREEN \
          CIRCLE, data=?";
         "id=3, plug=BLUE PENTAGON, leftSocket=BLUE CIRCLE, rightSocket=GREEN \
          CIRCLE, data=?";
         "id=4, plug=RED TRIANGLE, leftSocket=BLUE PENTAGON, rightSocket=GREEN \
          PENTAGON, data=?";
         "id=5, plug=RED PENTAGON, leftSocket=GREEN CIRCLE, rightSocket=GREEN \
          CIRCLE, data=?";
       ])
