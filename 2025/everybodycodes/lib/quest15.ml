open Base
open Util

type orientation = North | South | East | West
type direction = Left of int | Right of int

let parse_instructions s =
  s |> String.split ~on:','
  |> List.map ~f:(fun s ->
         let n =
           Int.of_string @@ String.sub s ~pos:1 ~len:(String.length s - 1)
         in
         match s.[0] with
         | 'L' -> Left n
         | 'R' -> Right n
         | _ -> failwith "bad input")

let delta facing dir =
  match (facing, dir) with
  | North, Left _ -> (-1, 0)
  | North, Right _ -> (1, 0)
  | South, Left _ -> (1, 0)
  | South, Right _ -> (-1, 0)
  | East, Left _ -> (0, -1)
  | East, Right _ -> (0, 1)
  | West, Left _ -> (0, 1)
  | West, Right _ -> (0, -1)

let next_facing facing dir =
  match (facing, dir) with
  | North, Left _ -> West
  | North, Right _ -> East
  | South, Left _ -> East
  | South, Right _ -> West
  | East, Left _ -> North
  | East, Right _ -> South
  | West, Left _ -> South
  | West, Right _ -> North

let rec follow_directions (x, y, facing, walls) directions =
  match directions with
  | [] -> ((0, 0), (x, y), Set.remove walls (x, y))
  | dir :: dl -> (
      let dx, dy = delta facing dir in
      let walk n =
        follow_directions
          ( x + (n * dx),
            y + (n * dy),
            next_facing facing dir,
            1 -- n
            |> List.fold ~init:walls ~f:(fun acc n ->
                   let nx, ny = (x + (n * dx), y + (n * dy)) in
                   Set.add acc (nx, ny)) )
          dl
      in
      match dir with Left n -> walk n | Right n -> walk n)

(* BFS from the given location should be enough for p1 *)

let cardinal_deltas = [ (0, 1); (1, 0); (-1, 0); (0, -1) ]

let bounds walls =
  Set.fold
    ~init:((0, 0), (0, 0))
    ~f:(fun ((xmin, ymin), (xmax, ymax)) (x, y) ->
      ((min xmin x, min ymin y), (max xmax x, max ymax y)))
    walls

let flood_fill distance_fn (start, goal, walls) =
  let queue, explored, distance, loop_done =
    ( Queue.create (),
      Hash_set.create (module IntPair),
      Hashtbl.create (module IntPair),
      ref false )
  in
  let (xmin, ymin), (xmax, ymax) = bounds walls in
  Queue.enqueue queue start;
  Hashtbl.add_exn distance ~key:start ~data:0;
  while (not (Queue.is_empty queue)) && not !loop_done do
    let next = Queue.dequeue_exn queue in
    if equal_tuple next goal then loop_done := true
    else
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem explored neighbor) then (
            Hash_set.add explored neighbor;
            Hashtbl.set distance ~key:neighbor
              ~data:(distance_fn next neighbor + Hashtbl.find_exn distance next);
            Queue.enqueue queue neighbor))
        (cardinal_deltas
        |> List.map ~f:(fun (dx, dy) -> (fst next + dx, snd next + dy))
        |> List.filter ~f:(fun c -> not (Set.mem walls c))
        |> List.filter ~f:(fun (x, y) ->
               x >= xmin && x <= xmax && y >= ymin && y <= ymax))
  done;
  (* Stdio.printf "bounds %s %s\n"
       (show_tuple (xmin, xmax))
       (show_tuple (ymin, ymax));
     for y = ymin to ymax do
       for x = xmin to xmax do
         Stdio.printf "%c"
           (if equal_tuple goal (x, y) then 'E'
            else if equal_tuple (0, 0) (x, y) then 'S'
            else if Set.mem walls (x, y) then '#'
            else if Hashtbl.mem distance (x, y) then '*'
            else '.')
       done;
       Stdio.printf "\n"
     done; *)
  Hashtbl.find_exn distance goal

let part1 s =
  parse_instructions s
  |> follow_directions (0, 0, North, Set.empty (module IntPair_Comparator))
  |> flood_fill (fun _ _ -> 1)

let%test_unit "part1 (given 1)" =
  [%test_eq: int] 6 (part1 "R3,R4,L3,L4,R3,R6,R9")

let%test_unit "part1 (given 2)" =
  [%test_eq: int] 16
    (part1 "L6,L3,L6,R3,L6,L3,L3,R6,L6,R6,L6,L6,R3,L3,L3,R3,R3,L6,L6,L3")

(*
we can no longer simulate the entire grid of course
we have a set of line segments.  we can sort it by x and y. then when we step in a direction
seems like we need to turn the space into a group of rectangles.
the last space is not in a rectangle, but that can be OK.  once we step into
the final area it is just a manhattan distance to the goal.
I am not really sure that I can get this working without getting bogged down
in a bunch of edge cases.  but it will be OK.
*)

let rec build_lines (x, y, facing, acc) directions =
  match directions with
  | [] -> ((x, y), acc)
  | dir :: dl -> (
      let dx, dy = delta facing dir in
      let walk n =
        let nx, ny = (x + (n * dx), y + (n * dy)) in
        build_lines
          (nx, ny, next_facing facing dir, ((x, y), (nx, ny)) :: acc)
          dl
      in
      match dir with Left n -> walk @@ n | Right n -> walk @@ n)

let build_compressed_walls lines (x_compressor, y_compressor) =
  let _compress_x, _compress_y =
    (Map.find_exn x_compressor, Map.find_exn y_compressor)
  in
  let tuple_compress (x, y) = (_compress_x x, _compress_y y) in
  List.fold
    ~init:(Set.empty (module IntPair_Comparator))
    ~f:(fun acc (start, _end) ->
      let start_c, end_c = (tuple_compress start, tuple_compress _end) in
      List.fold ~init:acc ~f:Set.add (tuple_range start_c end_c))
    lines

(* idea is that you sort the x-coords and the y-coords and
   then map x ---> idx in the list, search on x/y indices *)

let compression f l =
  l
  |> List.fold ~init:[] ~f:(fun acc (sc, ec) ->
         let x, y = (f sc, f ec) in
         (* needs elements right before s and right before e to simulate wall hugging *)
         x :: (x + 1) :: (x - 1) :: y :: (y + 1) :: (y - 1) :: acc)
  |> List.sort ~compare
  |> List.remove_consecutive_duplicates ~equal
  |> List.foldi
       ~init:(Map.empty (module Int))
       ~f:(fun n m i -> Map.add_exn m ~key:i ~data:n)

let map_invert_exn m map =
  Map.fold map ~init:(Map.empty m) ~f:(fun ~key ~data acc ->
      Map.add_exn acc ~key:data ~data:key)

let part3 s =
  let sq, lines = parse_instructions s |> build_lines (0, 0, North, []) in
  let _invert = map_invert_exn (module Int) in
  let x_compressor, y_compressor =
    (compression fst lines, compression snd lines)
  in
  let x_biggener, y_biggener = (_invert x_compressor, _invert y_compressor) in
  let start = (Map.find_exn x_compressor 0, Map.find_exn y_compressor 0) in
  let goal =
    (Map.find_exn x_compressor (fst sq), Map.find_exn y_compressor (snd sq))
  in
  let walls =
    Set.remove (build_compressed_walls lines (x_compressor, y_compressor)) goal
  in
  flood_fill
    (fun (sx, sy) (ex, ey) ->
      Int.abs (Map.find_exn x_biggener sx - Map.find_exn x_biggener ex)
      + Int.abs (Map.find_exn y_biggener sy - Map.find_exn y_biggener ey))
    (start, goal, walls)

let%test_unit "part3 (given for part1)" =
  [%test_eq: int] 16
    (part3 "L6,L3,L6,R3,L6,L3,L3,R6,L6,R6,L6,L6,R3,L3,L3,R3,R3,L6,L6,L3")
