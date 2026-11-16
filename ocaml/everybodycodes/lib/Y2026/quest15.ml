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
   cutoff potential, must clear all sulfer in an area before moving on *)
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
    if !x < xmax then columns := (start_x - 1, !x) :: !columns;
    (* for x' = start_x - 1 to !x do
      Stdio.printf "%c" (grid_at g (x', 1))
    done;
    Stdio.printf "\n"; *)
    x := !x + 1
  done;
  List.rev_map
    ~f:(fun (xstart, xend) -> ((xstart, 0), (xend, ymax - 1)))
    !columns

let%test_unit "parse columns" =
  [%test_result: ((int * int) * (int * int)) list]
    ~expect:[ ((0, 0), (12, 14)); ((12, 0), (29, 14)); ((29, 0), (51, 14)) ]
    (parse_columns (to_grid example))

let count_sulfer g ((xmin, ymin), (xmax, ymax)) =
  let indices =
    let open Sequence.Let_syntax in
    let%bind x = Sequence.range xmin xmax ~start:`inclusive ~stop:`inclusive in
    let%map y = Sequence.range ymin ymax ~start:`inclusive ~stop:`inclusive in
    (x, y)
  in
  Sequence.count indices ~f:(fun c -> equal_char (grid_at g c) '@')

let%test_unit "count_sulfer" =
  [%test_result: int list] ~expect:[ 5; 4; 7 ]
    (let g = to_grid example in
     List.map ~f:(count_sulfer g) (parse_columns g))

(* OK within these we can dijkstra more simply *)
(* let's start with DFS I guess *)

type search_state = {
  coord : int * int;
  num_sulfur : int;
  direction : int * int;
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
  | 0, 1 | 0, -1 -> Sequence.of_list [ (-1, 0); (1, 0) ]
  | 1, 0 | -1, 0 -> Sequence.of_list [ (0, 1); (0, -1) ]
  | _ -> invalid_arg "invalid dx, dy"

let in_bounds (x, y) ((xmin, ymin), (xmax, ymax)) =
  xmin <= x && x <= xmax && ymin <= y && y <= ymax

let neighbors g bounds ss =
  let x, y = ss.coord in
  let dx, dy = ss.direction in
  let coord_next = (x + dx, y + dy) in
  Sequence.append
    (if in_bounds coord_next bounds then
       match grid_at_opt g coord_next with
       | None -> Sequence.empty
       | Some ch ->
           Sequence.singleton
             {
               ss with
               num_sulfur = (ss.num_sulfur + if equal_char ch '@' then 1 else 0);
               coord = (x + dx, y + dy);
             }
     else Sequence.empty)
    (Sequence.map
       ~f:(fun dir -> { ss with direction = dir })
       (turns ss.direction))

(* direction is which dx, dy would be going forward *)

let bfs root goal ~neighbors =
  let q = Queue.create () in
  let visited = Hash_set.create (module SearchState_Comparator) in
  let dist = Hashtbl.create (module SearchState_Comparator) in
  let result = ref None in
  Hash_set.add visited root;
  Hashtbl.set dist ~key:root ~data:0;
  Queue.enqueue q root;
  while (not (Queue.is_empty q)) && Option.is_none !result do
    let next = Queue.dequeue_exn q in
    let curr_dist = Hashtbl.find_exn dist next in
    Stdio.printf "looking at %s\n" (show_search_state next);
    if equal_search_state next goal then
      result := Some (Hashtbl.find_exn dist goal)
    else
      Sequence.iter (neighbors next) ~f:(fun neighbor_state ->
          if not (Hash_set.mem visited neighbor_state) then begin
            Hash_set.add visited neighbor_state;
            Hashtbl.set dist ~key:neighbor_state ~data:(curr_dist + 1);
            Queue.enqueue q neighbor_state
          end)
    (* we are done *)
  done;
  !result

let search_quadrant g bounds =
  let (xmin, ymin), (xmax, ymax) = bounds in
  let start_node =
    { coord = (xmin, (ymax - ymin) / 2); num_sulfur = 0; direction = (1, 0) }
  in
  let end_node =
    {
      coord = (xmax, (ymax - ymin) / 2);
      num_sulfur = count_sulfer g bounds;
      direction = (1, 0);
    }
  in
  Stdio.printf "start %s ---> end %s\n"
    (show_search_state start_node)
    (show_search_state end_node);
  bfs start_node end_node ~neighbors:(neighbors g bounds)

let%test_unit "search_quadrants" =
  [%test_result: int option list] ~expect:[]
    (let g = to_grid example in
     List.map ~f:(search_quadrant g) (parse_columns g))

let part1 _ = 1
let part2 _ = 1
let part3 _ = 1
