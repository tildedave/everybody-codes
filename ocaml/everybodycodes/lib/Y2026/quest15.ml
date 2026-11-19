[@@@ocaml.warning "-27-26-31-32"]

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
[@@deriving compare, show, sexp_of, equal]

module SearchState = struct
  type t = search_state [@@deriving compare, show, sexp_of]

  let hash = Hashtbl.hash
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

  let hash = Hashtbl.hash
end

module SearchComparator = struct
  include Search
  include Comparator.Make (Search)
end

let dijkstra ?(print_trace = false) root ~is_goal ~neighbors ~should_cutoff =
  let dist = Hashtbl.create (module SearchState_Comparator) in
  let prev = Hashtbl.create (module SearchState_Comparator) in
  let result = ref None in
  let final = ref None in
  Hashtbl.set dist ~key:root ~data:0;
  let frontier = ref (Set.singleton (module SearchComparator) (0, root)) in
  while (not (Set.is_empty !frontier)) && Option.is_none !result do
    let ((curr_dist, next) as old) = Set.min_elt_exn !frontier in
    frontier := Set.remove !frontier old;
    if not (should_cutoff old) then
      if is_goal (curr_dist, next) then (
        result := Some curr_dist;
        final := Some next)
      else
        Sequence.iter (neighbors next) ~f:(fun neighbor_state ->
            match Hashtbl.find dist neighbor_state with
            | Some d' when d' <= curr_dist + 1 -> ()
            | old ->
                Option.iter
                  ~f:(fun d' ->
                    assert (Set.mem !frontier (d', neighbor_state));
                    frontier := Set.remove !frontier (d', neighbor_state))
                  old;
                frontier := Set.add !frontier (curr_dist + 1, neighbor_state);
                Hashtbl.set prev ~key:neighbor_state ~data:next;
                Hashtbl.set dist ~key:neighbor_state ~data:(curr_dist + 1))
    (* we are done *)
  done;
  if print_trace then
    while Option.is_some !final do
      Stdio.printf "%s\n" (show_search_state @@ Option.value_exn !final);
      final := Hashtbl.find prev (Option.value_exn !final)
    done;
  (!result, !final)

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
  dijkstra start_node
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

let in_corridor g (x, y) =
  equal_char (grid_at g (x, y + 1)) '#' && equal_char (grid_at g (x, y - 1)) '#'

let possible_starts g =
  let xmax, ymax = g.bounds in
  let y = ymax / 2 in
  let indices =
    Sequence.map ~f:(fun x -> (x, y)) (Sequence.range 0 (xmax - 1))
  in
  (* can start on any square that doesn't have #s above it
     this is incorrect (e.g. might be at the end of a loop), but those examples
     will quickly get winnowed out by the bounds check in the neighbors *)
  Sequence.filter ~f:(fun (x, y) -> not (in_corridor g (x, y))) indices

(*    let%test_unit "possible_starts" =
  let g = to_grid example in
  [%test_result: (int * int) list] ~expect:[]
    (Sequence.to_list (possible_starts (to_grid example))) *)
let walk_loop g (x, y) dy =
  let xmax, _ = g.bounds in
  let starting_x, starting_y = (x, y) in
  let x, y = (ref x, ref y) in
  (* walk right, walk up until we can't, walk right until we can't, walk
       down until we can walk right *)
  while not (equal_char (grid_at g (!x, !y + dy)) '#') do
    y := !y + dy
  done;
  let topy = !y in
  while not (equal_char (grid_at g (!x + 1, !y)) '#') do
    x := !x + 1
  done;
  while equal_char (grid_at g (!x + 1, !y)) '#' do
    y := !y - dy
  done;
  if equal_int starting_x !x then None
  else Some ((starting_x, min topy starting_y), (!x, max topy starting_y))

let upper_loop g (x, y) = walk_loop g (x, y) (-1)
let lower_loop g (x, y) = walk_loop g (x, y) 1

let full_loop g (x, y) =
  let open Option.Let_syntax in
  let%bind (starting_x, upper_y), (end_x, up_y) = walk_loop g (x, y) (-1) in
  let%map (_, lower_y), (end_x, down_y) = walk_loop g (x, y) 1 in
  let r = ((starting_x, upper_y), (end_x, down_y)) in
  Stdio.printf "full loop is %s %s upper_y=%d lower_y=%d down_y=%d up_y=%d\n"
    (show_tuple (fst r))
    (show_tuple (snd r))
    upper_y lower_y down_y up_y;
  r

(* we're going to assume that upper/lower loop are all that's needed, e.g. no
   walking upper, going to lower, etc.  could clearly be wrong *)

(* let%test_unit "loop_end matches our parsed columns" =
  let g = to_grid example in
  let columns = parse_columns g in
  [%test_result: ((int * int) * (int * int)) option list]
    ~expect:[]
      (* (List.map
         ~f:(fun ((_, ymin), (xmax, ymax)) -> Some (xmax, (ymax - ymin) / 2))
         columns) *)
    (List.map
       ~f:(fun ((xmin, ymin), (_, ymax)) ->
         upper_loop g (xmin, (ymax - ymin) / 2))
       columns) *)

let add2 (x, y) (dx, dy) = (x + dx, y + dy)

let search_bounds g lstart bounds =
  let sulfur_spots = find_sulfur g bounds in
  Stdio.printf "bounds are %s, %s - %d sulfur available\n"
    (show_tuple (fst bounds))
    (show_tuple (snd bounds))
    (Sequence.length sulfur_spots);
  _print_subgrid g bounds;
  let sulfur_map =
    Map.of_alist_exn
      (module IntPair_Comparator)
      (Sequence.to_list (Sequence.mapi ~f:(fun i c -> (c, i)) sulfur_spots))
  in
  let start_node =
    { coord = lstart; sulfur = 0; direction = (1, 0); num_sulfurs = 0 }
  in
  (* of course no reason to search for 0 sulfur *)
  let sulfer_rng =
    Sequence.range 1 ~stop:`inclusive (Sequence.length sulfur_spots)
  in
  let open Sequence.Let_syntax in
  let%map i = sulfer_rng in
  (* we just won't ask for sulfur clearance *)
  let end_node =
    { coord = lstart; sulfur = 0; direction = (1, 0); num_sulfurs = i }
  in
  (* Stdio.printf "start %s ---> end %s\n"
       (show_search_state start_node)
       (show_search_state end_node); *)
  dijkstra start_node
    ~neighbors:(neighbors g bounds sulfur_map ~part1:true)
    ~is_goal:(fun (_, next) ->
      equal_search_state next { end_node with direction = next.direction })
    ~should_cutoff:(fun _ -> false)
  |> fst
  |> Option.map ~f:(fun n -> (n, i))

(* let loop_time g lstart =
  let f = fun o -> o |> Option.map ~f:(search_bounds g lstart) |> Option.join in
  [ upper_loop g lstart |> f; lower_loop g lstart |> f ] |> List.filter_opt *)

type loop_result = {
  seconds : int;
  num_sulfur : int;
  start : int * int;
  bounds : (int * int) * (int * int);
}
[@@deriving sexp_of, show]

let compare_loop_result result1 result2 =
  match (result1.num_sulfur, result2.num_sulfur) with
  | 0, 0 -> compare_tuple result1.start result2.start
  | 0, _ -> 1
  | _, 0 -> -1
  (* compare their ratios *)
  | _, _ -> (
      match
        compare_float
          (result1.num_sulfur // result1.seconds)
          (result2.num_sulfur // result2.seconds)
      with
      | 0 -> compare_tuple result1.start result2.start
      | r -> r)

let search_result g lstart bounds =
  search_bounds g lstart bounds
  |> Sequence.filter_opt
  |> Sequence.map ~f:(fun (time, num_sulfur) ->
      { seconds = time; num_sulfur; start = lstart; bounds })

(* actually we need to allow a loop that includes both upper and lower *)
let loop_time g lstart =
  let open Option.Let_syntax in
  let x =
    Sequence.of_list
      [
        (let%map upper_bounds = upper_loop g lstart in
         search_result g lstart upper_bounds);
        (let%map lower_bounds = lower_loop g lstart in
         search_result g lstart lower_bounds);
        (let%map full_bounds = full_loop g lstart in
         search_result g lstart full_bounds);
      ]
  in
  x |> Sequence.filter_opt |> Sequence.concat
  |> Sequence.filter ~f:(fun r -> r.num_sulfur > 0)

(* both start and end are in the corridor so we must move them inwards *)
(* OK this works, now we need to include the bounds, bleh *)

(* let%test_unit "making loop_time work" =
  let g = to_grid example in
  let columns = parse_columns g in
  [%test_result: loop_result list] ~expect:[]
    (List.concat
       (Sequence.to_list (Sequence.map ~f:(loop_time g) (possible_starts g)))) *)

let best_loop g =
  possible_starts g
  |> Sequence.concat_map ~f:(loop_time g)
  |> Sequence.to_list
  |> List.sort ~compare:compare_loop_result
  |> List.map ~f:(fun r ->
      Stdio.printf "loop result --> %s\n" (show_loop_result r);
      r)
  |> List.max_elt ~compare:compare_loop_result

let%test_unit "best_loop" =
  let g = to_grid example in
  let columns = parse_columns g in
  [%test_result: loop_result option]
    ~expect:
      (Some
         {
           seconds = 8;
           num_sulfur = 3;
           start = (5, 7);
           bounds = ((5, 5), (7, 7));
         })
    (best_loop g)

let part2_search (g : grid) =
  let xmax, ymax = g.bounds in
  let bounds = ((0, 0), (xmax - 1, ymax - 1)) in
  let start_node =
    {
      coord = (0, (ymax - 1) / 2);
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
  let end_node =
    {
      coord = (xmax, (ymax - 0) / 2);
      sulfur = 0;
      direction = (1, 0);
      num_sulfurs = 2026;
    }
  in
  let best_for_coord = Hashtbl.create (module SearchState_Comparator) in
  (* Stdio.printf "start %s ---> end %s\n"
        (show_search_state start_node)
        (show_search_state end_node); *)
  fst
  @@ dijkstra start_node
       ~is_goal:(fun (_, next) ->
         equal_search_state next
           { end_node with direction = next.direction; sulfur = next.sulfur })
       ~neighbors:(neighbors g bounds sulfur_map)
       ~should_cutoff:(fun (d, ss) ->
         if ss.num_sulfurs > 2026 then true
         else
           match Hashtbl.find best_for_coord ss with
           | Some d' when d' < d -> true
           | _ ->
               Hashtbl.set best_for_coord ~key:ss ~data:d;
               false)

let part3_search g goal_num =
  let lr = Option.value_exn (best_loop g) in
  let (startx, _), (_, _) = lr.bounds in
  let xmax, ymax = g.bounds in
  (* so actually the fudge value needs to just be > the sulfur between the start *)
  let fudge = 500 in
  let num_loop_executions = ((goal_num - fudge) / lr.num_sulfur) - 4 in
  (* we actually start with a time = time to get to lr.start + time to execute loop a billion times, *)
  let actual_start_node =
    { coord = (0, ymax / 2); sulfur = 0; direction = (1, 0); num_sulfurs = 0 }
  in
  let begin_loop_node =
    { coord = lr.start; sulfur = 0; direction = (1, 0); num_sulfurs = fudge }
  in
  (* technically we can push the bounds forward, OK, let's do that *)
  let bounds = ((0, 0), (xmax - 1, ymax - 1)) in
  let sulfur_spots = find_sulfur g bounds in
  let sulfur_map =
    Map.of_alist_exn
      (module IntPair_Comparator)
      (Sequence.to_list (Sequence.mapi ~f:(fun i c -> (c, i)) sulfur_spots))
  in
  let open Option.Let_syntax in
  let get_to_start_opt, start_state_opt =
    dijkstra actual_start_node
      ~is_goal:(fun (_, next) ->
        if
          next.num_sulfurs >= fudge
          && equal_search_state next
               {
                 begin_loop_node with
                 direction = next.direction;
                 sulfur = next.sulfur;
                 num_sulfurs = next.num_sulfurs;
               }
        then true
        else false)
      ~neighbors:(neighbors g bounds sulfur_map)
      ~should_cutoff:(fun _ -> false)
  in
  let%bind start_state = start_state_opt in
  let%bind get_to_start = get_to_start_opt in
  let start_node =
    {
      coord = lr.start;
      sulfur = 0;
      direction = (1, 0);
      num_sulfurs =
        start_state.num_sulfurs + (lr.num_sulfur * num_loop_executions);
    }
  in
  let end_node =
    {
      coord = (xmax - 1, ymax / 2);
      sulfur = 0;
      direction = (1, 0);
      num_sulfurs = goal_num;
    }
  in
  Stdio.printf "actual start %s\n" (show_search_state actual_start_node);
  Stdio.printf "get to start %d\n" get_to_start;
  Stdio.printf "start from %s\n" (show_search_state start_node);
  Stdio.printf "goal is %s\n" (show_search_state end_node);
  Stdio.printf "best is %s\n" (show_loop_result lr);
  dijkstra start_node
    ~is_goal:(fun (_, next) ->
      next.num_sulfurs >= goal_num && equal_tuple end_node.coord next.coord)
    ~neighbors:(neighbors g bounds sulfur_map)
    ~should_cutoff:(fun (_, next) -> next.num_sulfurs > goal_num)
  |> fst
  |> Option.map ~f:(fun end_time ->
      get_to_start + (num_loop_executions * lr.seconds) + end_time)

let part2 l =
  let g = to_grid l in
  Option.value_exn @@ part3_search g 2026

let%test_unit "part2 (given example)" =
  [%test_result: int] ~expect:5448 (part2 example)

let part3 l =
  let g = to_grid l in
  Option.value_exn @@ part3_search g 202620262026

let%test_unit "part3 (given example)" =
  [%test_result: int] ~expect:540320698784 (part3 example)

(*5 + (674 * 8) + 6 = 5403*)
