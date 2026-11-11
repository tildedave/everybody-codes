open Base
open Util

(* OK we need to parse the lines into components.
   we can be quite stupid, I think *)

let _next_component_id = ref 0

let component_id () =
  _next_component_id := !_next_component_id + 1;
  !_next_component_id

let rec grid_fill g component_num ~m coord =
  List.fold
    ~init:(Map.set m ~key:coord ~data:component_num)
    ~f:(fun m neighbor ->
      if (not (Map.mem m neighbor)) && equal_char (grid_at g neighbor) '#' then
        (* Stdio.printf "visit coord %s\n" (show_tuple neighbor); *)
        grid_fill g component_num ~m neighbor
      else m)
    (grid_cardinal_neighbors g coord)

let fill_components g =
  List.fold
    ~init:(Map.empty (module IntPair_Comparator))
    ~f:(fun m coord ->
      if Map.mem m coord || not (equal_char (grid_at g coord) '#') then m
      else grid_fill g (component_id ()) ~m coord)
    (grid_all_coords g)

let num_reflections m component_num =
  let coords = Map.keys @@ Map.filter m ~f:(equal component_num) in
  let xmax =
    fst @@ Option.value_exn
    @@ List.max_elt ~compare:(Comparable.lift compare ~f:fst) coords
  in
  let xmin =
    fst @@ Option.value_exn
    @@ List.min_elt ~compare:(Comparable.lift compare ~f:fst) coords
  in
  let ymax =
    snd @@ Option.value_exn
    @@ List.max_elt ~compare:(Comparable.lift compare ~f:snd) coords
  in
  let ymin =
    snd @@ Option.value_exn
    @@ List.min_elt ~compare:(Comparable.lift compare ~f:snd) coords
  in
  (* Stdio.printf "component %d bounds are %s %s\n" component_num
    (show_tuple (xmin, ymin))
    (show_tuple (xmax, ymax)); *)
  min (ymax - ymin + 1) (xmax - xmin + 1)

let total m =
  m |> Map.data
  |> Set.of_list (module Int)
  |> Set.fold ~init:0 ~f:(fun n id -> n + num_reflections m id)

let part1 l =
  let g = to_grid l in
  total @@ fill_components g

let example =
  [
    "........#.";
    ".##.......";
    ".##..####.";
    ".##..####.";
    ".....####.";
    ".###......";
  ]

let%test_unit "part1 (given)" = [%test_result: int] ~expect:7 (part1 example)
let part2 _ = 1
let part3 _ = 1
