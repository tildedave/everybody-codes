open Base
open Util

(* OK we need to parse the lines into components.
   we can be quite stupid, I think *)

let _next_component_id = ref 0

let component_id () =
  _next_component_id := !_next_component_id + 1;
  !_next_component_id

let rec grid_fill g component_num ~m coord ~f =
  List.fold
    ~init:(Map.set m ~key:coord ~data:component_num)
    ~f:(fun m neighbor ->
      if (not (Map.mem m neighbor)) && f neighbor (grid_at g neighbor) then
        (* Stdio.printf "visit coord %s\n" (show_tuple neighbor); *)
        grid_fill g component_num ~m neighbor ~f
      else m)
    (grid_cardinal_neighbors g coord)

let fill_components g =
  List.fold
    ~init:(Map.empty (module IntPair_Comparator))
    ~f:(fun m coord ->
      if Map.mem m coord || not (equal_char (grid_at g coord) '#') then m
      else grid_fill g (component_id ()) ~m coord ~f:(fun _ -> equal_char '#'))
    (grid_all_coords g)

let bounding_box coords =
  if List.is_empty coords then None
  else
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
    Some ((xmin, ymin), (xmax, ymax))

let component_coords m component_num =
  Map.keys @@ Map.filter m ~f:(equal component_num)

let num_reflections ((xmin, ymin), (xmax, ymax)) =
  min (ymax - ymin + 1) (xmax - xmin + 1)

let components m = m |> Map.data |> Set.of_list (module Int)

let total m =
  components m
  |> Set.fold ~init:0 ~f:(fun n id ->
      n
      + num_reflections
          (Option.value_exn @@ bounding_box (component_coords m id)))

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

let in_bbox ((xmin, ymin), (xmax, ymax)) (x, y) =
  xmin <= x && x <= xmax && ymin <= y && y <= ymax

let indices ((xmin, ymin), (xmax, ymax)) =
  let open Sequence.Let_syntax in
  Stdio.printf "%d %d %d %d\n" xmin ymin xmax ymax;
  let%bind y = Sequence.range ~stop:`inclusive ymin ymax in
  let%map x = Sequence.range ~stop:`inclusive xmin xmax in
  (x, y)

(* part 2 seems annoying *)
(* OK, we can determine the bounding box and carve out the size of the empty
   square inside it, using a similar flood fill method *)
(* OK then *)
let empty_bounding_box g bbox =
  indices bbox
  |> Sequence.fold
       ~init:(Map.empty (module IntPair_Comparator))
       ~f:(fun m coord ->
         if Map.mem m coord || not (equal_char (grid_at g coord) '.') then m
         else
           grid_fill g (component_id ()) ~m coord ~f:(fun (x, y) ch ->
               in_bbox bbox (x, y) && equal_char '.' ch))
  |> Map.keys |> bounding_box

let p2_example1 =
  [
    "........#.";
    ".##.......";
    ".##..####.";
    ".#...#....";
    ".....#....";
    ".####.....";
  ]

(* we split each component into 2 pieces based on the empty bounding box (how
   to do this generically?) *)
(* just iterate everything in the bounding box, the x, y outside the ebb etc *)

let split_shapes bb ebb =
  let (xmin, ymin), (xmax, ymax) = ebb in
  indices bb
  |> Sequence.fold
       ~init:
         ( Set.empty (module IntPair_Comparator),
           Set.empty (module IntPair_Comparator) )
       ~f:(fun (one, two) (x, y) ->
         if in_bbox ebb (x, y) then (one, two)
         else if (xmin <= x && x <= xmax) || (ymin <= y && y <= ymax) then
           (one, Set.add two (x, y))
         else (Set.add one (x, y), two))
  |> fun (one, two) ->
  Stdio.printf "bb (%s, %s)\n"
    (show_tuple @@ fst @@ Option.value_exn @@ bounding_box (Set.to_list one))
    (show_tuple @@ snd @@ Option.value_exn @@ bounding_box (Set.to_list one));

  ( num_reflections @@ Option.value_exn @@ bounding_box (Set.to_list one),
    num_reflections @@ Option.value_exn @@ bounding_box (Set.to_list two) )

let part2 l =
  let g = to_grid l in
  let m = fill_components g in
  components m
  |> Set.iter ~f:(fun component_num ->
      let bb =
        Option.value_exn @@ bounding_box @@ component_coords m component_num
      in
      let ebb = empty_bounding_box g bb in
      Stdio.printf "component %d - bounding box is (%s, %s)" component_num
        (show_tuple (fst bb))
        (show_tuple (snd bb));
      if Option.is_some ebb then begin
        Stdio.printf " empty bb is (%s, %s)\n"
          (show_tuple (fst @@ Option.value_exn ebb))
          (show_tuple (snd @@ Option.value_exn ebb));
        let one, two = split_shapes bb (Option.value_exn ebb) in
        Stdio.printf "lasers: %d\n" (one + two)
      end
      else Stdio.printf "\nlasers: %d\n" (num_reflections bb);
      Stdio.printf "\n")
  |> fun _ -> 1

let%test_unit "part2 (given)" =
  [%test_result: int] ~expect:7 (part2 p2_example1)

let part3 _ = 1
