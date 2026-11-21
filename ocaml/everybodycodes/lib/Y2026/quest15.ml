open Base
open Util

let example =
  [
    "#####################################################";
    "#.@.........#................#.....................##";
    "#.#########@#.##############.#.###################.##";
    "#.#.......#.#.##..........##.#.###..@..@......@###.##";
    "#.#.#####.#.#.##.########.##.#.###@###########.###.##";
    "#.#.#@@@#.#.#.##.##....##.##@#.###.###.....###.###.##";
    "#.#.#.#.#.#.#.##.##.##.##.##.#.###.###.###.###.###.##";
    "S...................................................E";
    "#.#.#.#.#.#.#.##.##.##.##.##.#.###.###.###.###.###@##";
    "#.#.#...#.#.#.##@##.@..##.##.#.###.###@....###.###.##";
    "#.#.#####.#.#.##.########.##.#.###.###########.###.##";
    "#.#.......#.#.##..@.......##.#.###.............###.##";
    "#.#########.#.##############.#.###################.##";
    "#...........#................#................@....##";
    "#####################################################";
  ]

(* of course this is just dijkstra.  seems like there should be a lot of
   cutoff potential, must clear all sulfur in an area before moving on *)
(* we can search quadrants individually. *)

let parse_columns g =
  (* let indices = Sequence.map ~f:(fun x -> (x, 1)) (Sequence.range 1 (fst g.bounds)) in *)
  let columns = ref [] in
  let xmax, ymax = g.bounds in
  let x = ref 1 in
  while !x < xmax - 1 do
    let start_x = !x in
    while (not (equal_char (grid_at g (!x, 1)) '#')) && !x < xmax do
      x := !x + 1
    done;
    while !x + 1 < xmax && equal_char (grid_at g (!x + 1, 1)) '#' do
      x := !x + 1
    done;
    if !x < xmax then columns := (start_x - 1, !x) :: !columns;
    x := !x + 1
  done;
  List.rev_map
    ~f:(fun (xstart, xend) -> ((xstart, 0), (xend, ymax - 1)))
    !columns

let%test_unit "parse columns" =
  [%test_result: ((int * int) * (int * int)) list]
    ~expect:[ ((0, 0), (12, 14)); ((12, 0), (29, 14)); ((29, 0), (52, 14)) ]
    (parse_columns (to_grid example))

let find_sulfur g ((xmin, ymin), (xmax, ymax)) =
  let indices =
    let open Sequence.Let_syntax in
    let%bind x = Sequence.range xmin xmax ~start:`inclusive ~stop:`inclusive in
    let%map y = Sequence.range ymin ymax ~start:`inclusive ~stop:`inclusive in
    (x, y)
  in
  Sequence.filter indices ~f:(fun c -> equal_char (grid_at g c) '@')

let count_sulfur g bounds = find_sulfur g bounds |> Sequence.length

let%test_unit "count_sulfur" =
  [%test_result: int list] ~expect:[ 5; 4; 7 ]
    (let g = to_grid example in
     List.map ~f:(count_sulfur g) (parse_columns g))

(* OK within these we can dijkstra more simply *)
(* let's start with DFS I guess *)

(* steal from quest10 last year *)
let set_bit bb i = (1 lsl i) lor bb
let _unset_bit bb i = lnot (1 lsl i) land bb
let is_bit_set bb i = not @@ equal ((bb lsr i) land 1) 0

type search_state = {
  coord : int * int;
  sulfur : int; [@compare.ignore] [@equal.ignore] [@hash.ignore]
  direction : int * int;
  num_sulfurs : int;
}
[@@deriving compare, show, sexp_of, equal, hash]

module SearchState = struct
  type t = search_state [@@deriving compare, equal, show, sexp_of, hash]
end

module SearchState_Comparator = struct
  include SearchState
  include Comparator.Make (SearchState)
end

let turns (dx, dy) =
  match (dx, dy) with
  | 0, 1 | 0, -1 -> Sequence.of_list [ (dx, dy); (-1, 0); (1, 0) ]
  | 1, 0 | -1, 0 -> Sequence.of_list [ (dx, dy); (0, 1); (0, -1) ]
  | _ -> invalid_arg "invalid dx, dy"

let in_bounds (x, y) ((xmin, ymin), (xmax, ymax)) =
  xmin <= x && x <= xmax && ymin <= y && y <= ymax

let neighbors ?(part1 = false) g bounds sulfur_map ss =
  let x, y = ss.coord in
  Sequence.filter_map
    ~f:(fun (dx, dy) ->
      let coord_next = (x + dx, y + dy) in
      if in_bounds coord_next bounds then
        match grid_at_opt g coord_next with
        | None -> None
        | Some ch ->
            if equal_char ch '#' then None
            else if equal_char ch '@' then
              let sulfur_idx = Map.find_exn sulfur_map (x + dx, y + dy) in
              if is_bit_set ss.sulfur sulfur_idx && part1 then None
              else
                Some
                  {
                    sulfur = set_bit ss.sulfur sulfur_idx;
                    coord = (x + dx, y + dy);
                    direction = (dx, dy);
                    num_sulfurs = ss.num_sulfurs + 1;
                  }
            else Some { ss with coord = (x + dx, y + dy); direction = (dx, dy) }
      else None)
    (turns ss.direction)

let%test_unit "neighbors" =
  [%test_result: search_state list]
    ~expect:
      [ { coord = (1, 7); direction = (1, 0); sulfur = 0; num_sulfurs = 0 } ]
    (let g = to_grid example in
     Sequence.to_list
       (neighbors g ~part1:true
          ((0, 0), (12, 14))
          (Map.empty (module IntPair_Comparator))
          { coord = (0, 7); direction = (1, 0); sulfur = 0; num_sulfurs = 0 }))

(* direction is which dx, dy would be going forward *)

(* let distance next neighbor_state =
  if equal_tuple next.coord neighbor_state.coord then 0 else 1 *)

module Search = struct
  type t = int * search_state [@@deriving compare, sexp_of, show]
end

module SearchComparator = struct
  include Search
  include Comparator.Make (Search)
end

let search_quadrant g bounds =
  let (xmin, ymin), (xmax, ymax) = bounds in
  let start_node =
    {
      coord = (xmin, (ymax - ymin) / 2);
      sulfur = 0;
      direction = (1, 0);
      num_sulfurs = 0;
    }
  in
  let sulfur_spots = find_sulfur g bounds in
  let sulfur_map =
    Map.of_alist_exn
      (module IntPair_Comparator)
      (Sequence.to_list (Sequence.mapi ~f:(fun i c -> (c, i)) sulfur_spots))
  in
  let goal_sulfur =
    List.fold ~init:0 ~f:set_bit (List.range 0 (Sequence.length sulfur_spots))
  in
  let end_node =
    {
      coord = (xmax, (ymax - ymin) / 2);
      sulfur = goal_sulfur;
      direction = (1, 0);
      num_sulfurs = Map.length sulfur_map;
    }
  in
  (* Stdio.printf "start %s ---> end %s\n"
    (show_search_state start_node)
    (show_search_state end_node); *)
  dijkstra
    (module SearchState)
    start_node
    ~is_goal:(fun (_, next) ->
      equal_search_state next { end_node with direction = next.direction })
    ~neighbors:(neighbors ~part1:true g bounds sulfur_map)
    ~should_cutoff:(fun _ -> false)
  |> fst

let%test_unit "search_quadrants" =
  [%test_result: int option list]
    ~expect:[ Some 40; Some 65; Some 79 ]
    (let g = to_grid example in
     List.map ~f:(search_quadrant g) (parse_columns g))

let part1 l =
  let g = to_grid l in
  List.sum
    (module Int)
    ~f:(Option.value ~default:0)
    (List.map ~f:(search_quadrant g) (parse_columns g))

let%test_unit "part1 (given example)" =
  [%test_result: int] ~expect:184 (part1 example)

(* part 2 can be converted to linear programming *)
(* for each quadrant, find the fastest way to get each n is a variable *)
(* sum of variables times each how much they will bring is = 2026 *)
(* sum of each variable quadrant >= 1 *)

(* quadrants are not quite so simple here.  it is easiest for us to only deal
   with sections that are pure loops *)
(* OK, we'll try with smaller start/end quadrants.  finding the starts is
   interesting.  I guess we can try having start/end being the same coords, yes.
   we would then need to find all possible 2^n sulfur possibilities inside
   that.  we can "catch" sulfur looping back so I think this does matter
*)

let _print_subgrid g ((xmin, ymin), (xmax, ymax)) =
  Stdio.printf "------- SUBGRID ------ %s %s\n"
    (show_tuple (xmin, ymin))
    (show_tuple (xmax, ymax));
  for y = ymin to ymax do
    for x = xmin to xmax do
      Option.iter (grid_at_opt g (x, y)) ~f:(Stdio.printf "%c")
    done;
    Stdio.printf "\n"
  done

type loop_result = {
  seconds : int;
  num_sulfur : int;
  start : int * int; (* bounds : (int * int) * (int * int); *)
  x_distance : int;
}
[@@deriving sexp_of, show]

let walk_loop g (x, y) dy =
  let starting_x, starting_y = (x, y) in
  let num_sulfur = ref 0 in
  let steps = ref 0 in
  if equal_char (grid_at g (x, y + dy)) '#' then None
  else
    let x, y = (ref x, ref y) in
    (* walk right, walk up until we can't, walk right until we can't, walk
       down until we can walk right *)
    while not (equal_char (grid_at g (!x, !y + dy)) '#') do
      y := !y + dy;
      steps := !steps + 1;
      if equal_char (grid_at g (!x, !y)) '@' then num_sulfur := !num_sulfur + 1
    done;
    while not (equal_char (grid_at g (!x + 1, !y)) '#') do
      x := !x + 1;
      steps := !steps + 1;
      if equal_char (grid_at g (!x, !y)) '@' then num_sulfur := !num_sulfur + 1
    done;
    while
      (not (equal_char (grid_at g (!x, !y - dy)) '#'))
      && not (equal_int starting_y !y)
    do
      y := !y - dy;
      steps := !steps + 1;
      if equal_char (grid_at g (!x, !y)) '@' then num_sulfur := !num_sulfur + 1
    done;
    if equal_int starting_x !x || not (equal_int starting_y !y) then None
    else
      Some
        {
          seconds = !steps + (!x - starting_x);
          start = (starting_x, starting_y);
          x_distance = !x - starting_x;
          num_sulfur = !num_sulfur;
        }

let upper_loop g (x, y) = walk_loop g (x, y) (-1)
let lower_loop g (x, y) = walk_loop g (x, y) 1

let full_loop g (x, y) =
  let open Option.Let_syntax in
  let%bind upper = upper_loop g (x, y) in
  let%map lower = lower_loop g (x, y) in
  {
    start = upper.start;
    num_sulfur = upper.num_sulfur + lower.num_sulfur;
    x_distance = upper.x_distance;
    seconds = upper.seconds + lower.seconds - (2 * upper.x_distance);
  }

let compare_loop_result result1 result2 =
  match (result1.num_sulfur, result2.num_sulfur) with
  | 0, 0 -> compare_tuple result1.start result2.start
  | 0, _ -> -1
  | _, 0 -> 1
  (* compare their ratios *)
  | _, _ -> (
      match
        compare_float
          (result1.num_sulfur // result1.seconds)
          (result2.num_sulfur // result2.seconds)
      with
      | 0 -> compare_tuple result1.start result2.start
      | r -> r)

let best_loop g =
  let xmax, ymax = g.bounds in
  Sequence.map ~f:(fun x -> (x, ymax / 2)) (Sequence.range 0 (xmax - 2))
  |> Sequence.concat_map ~f:(fun coord ->
      Sequence.of_list
        [ upper_loop g coord; lower_loop g coord; full_loop g coord ])
  |> Sequence.filter_opt
  |> Sequence.filter ~f:(fun lr -> lr.num_sulfur > 0)
  |> Sequence.to_list
  |> List.sort ~compare:compare_loop_result
  |> List.max_elt ~compare:compare_loop_result

let%test_unit "best_loop" =
  let g = to_grid example in
  [%test_result: loop_result option]
    ~expect:
      (Some { seconds = 8; num_sulfur = 3; start = (5, 7); x_distance = 3 })
    (best_loop g)

let part3_search ?(exact = false) g goal_num =
  let lr = Option.value_exn (best_loop g) in
  let xmax, ymax = g.bounds in
  (* so actually the fudge value needs to just be > the sulfur between the start *)
  let fudge = lr.num_sulfur * 2 in
  let num_loop_executions = (goal_num - fudge) / lr.num_sulfur in
  (* we actually start with a time = time to get to lr.start + time to execute loop a billion times, *)
  let bounds = ((0, 0), (xmax - 1, ymax - 1)) in
  let sulfur_spots = find_sulfur g bounds in
  let sulfur_map =
    Map.of_alist_exn
      (module IntPair_Comparator)
      (Sequence.to_list (Sequence.mapi ~f:(fun i c -> (c, i)) sulfur_spots))
  in
  let start_node =
    { coord = (0, ymax / 2); sulfur = 0; direction = (1, 0); num_sulfurs = 0 }
  in
  let target = goal_num - (num_loop_executions * lr.num_sulfur) in
  let end_node =
    {
      coord = (xmax - 1, ymax / 2);
      sulfur = 0;
      direction = (1, 0);
      num_sulfurs = target;
    }
  in
  dijkstra
    (module SearchState)
    start_node
    ~is_goal:(fun (_, next) ->
      (if exact then equal_int next.num_sulfurs target
       else next.num_sulfurs >= target)
      && equal_tuple end_node.coord next.coord)
    ~neighbors:(neighbors g bounds sulfur_map)
    ~should_cutoff:(fun (_, next) ->
      if exact then next.num_sulfurs > target else false)
  |> fst
  |> Option.map ~f:(fun end_time ->
      end_time + (num_loop_executions * lr.seconds))

let part2 l =
  let g = to_grid l in
  Option.value_exn @@ part3_search g 2026 ~exact:false

let%test_unit "part2 (given example)" =
  [%test_result: int] ~expect:5448 (part2 example)

let part3 l =
  let g = to_grid l in
  Option.value_exn @@ part3_search g 202620262026 ~exact:true

let%test_unit "part3 (given example)" =
  [%test_result: int] ~expect:540320698784 (part3 example)

(*5 + (674 * 8) + 6 = 5403*)
