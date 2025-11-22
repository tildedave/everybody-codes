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
  | [] -> ((x, y), Set.remove walls (x, y))
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

type tuple = int * int [@@deriving eq, show]
type float_tuple = float * float [@@deriving show]

let _ = show_tuple
let _ = show_float_tuple

let bounds walls =
  Set.fold
    ~init:((0, 0), (0, 0))
    ~f:(fun ((xmin, ymin), (xmax, ymax)) (x, y) ->
      ((min xmin x, min ymin y), (max xmax x, max ymax y)))
    walls

let flood_fill (sq, walls) =
  let queue, explored, distance, loop_done =
    ( Queue.create (),
      Hash_set.create (module IntPair),
      Hashtbl.create (module IntPair),
      ref false )
  in
  let (xmin, ymin), (xmax, ymax) = bounds walls in
  Queue.enqueue queue (0, 0);
  Hashtbl.add_exn distance ~key:(0, 0) ~data:0;
  while (not (Queue.is_empty queue)) && not !loop_done do
    let next = Queue.dequeue_exn queue in
    if equal_tuple next sq then loop_done := true
    else
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem explored neighbor) then (
            Hash_set.add explored neighbor;
            Hashtbl.set distance ~key:neighbor
              ~data:(1 + Hashtbl.find_exn distance next);
            Queue.enqueue queue neighbor))
        (cardinal_deltas
        |> List.map ~f:(fun (dx, dy) -> (fst next + dx, snd next + dy))
        |> List.filter ~f:(fun sq -> not (Set.mem walls sq))
        |> List.filter ~f:(fun (x, y) ->
               x >= xmin && x <= xmax && y >= ymin && y <= ymax))
  done;
  (* Stdio.printf "bounds %s %s\n"
       (show_tuple (xmin, xmax))
       (show_tuple (ymin, ymax));
     for y = ymin to ymax do
       for x = xmin to xmax do
         Stdio.printf "%c"
           (if equal_tuple (x, y) sq then 'E'
            else if equal_tuple (0, 0) (x, y) then 'S'
            else if Set.mem walls (x, y) then '#'
            else if Hashtbl.mem distance (x, y) then '*'
            else '.')
       done;
       Stdio.printf "\n"
     done; *)
  Hashtbl.find_exn distance sq

let part1 s =
  parse_instructions s
  |> follow_directions (0, 0, North, Set.empty (module IntPair_Comparator))
  |> flood_fill

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

let rec build_walls (x, y, facing, acc) directions =
  match directions with
  | [] -> ((x, y), Set.remove acc (x, y))
  | dir :: dl -> (
      let dx, dy = delta facing dir in
      let walk n =
        let nx, ny = (x + (n * dx), y + (n * dy)) in
        build_walls
          ( nx,
            ny,
            next_facing facing dir,
            List.fold ~init:acc ~f:Set.add [ (x, y); (nx, ny) ] )
          dl
      in
      match dir with Left n -> walk @@ n | Right n -> walk @@ n)

(* idea is that you sort the x-coords and the y-coords and
   then map x ---> idx in the list, search on x/y indices *)

let compression f l =
  l
  |> Set.fold ~init:[] ~f:(fun acc c ->
         let x = f c in
         (* needs elements right before s and right before e to simulate wall hugging *)
         x :: (x + 1) :: (x - 1) :: acc)
  |> List.sort ~compare
  |> List.remove_consecutive_duplicates ~equal
  |> List.foldi
       ~init:(Map.empty (module Int))
       ~f:(fun n m i -> Map.add_exn m ~key:i ~data:n)

let map_invert_exn m map =
  Map.fold map ~init:(Map.empty m) ~f:(fun ~key ~data acc ->
      Map.add_exn acc ~key:data ~data:key)

let flood_fill_compressed start goal walls
    (x_compressor, y_compressor, x_biggener, y_biggener) =
  let queue, explored, distance, loop_done =
    ( Queue.create (),
      Hash_set.create (module IntPair),
      Hashtbl.create (module IntPair),
      ref false )
  in
  let _compress_x, _compress_y =
    (Map.find_exn x_compressor, Map.find_exn y_compressor)
  in
  let _expand_x, _expand_y =
    (Map.find_exn x_biggener, Map.find_exn y_biggener)
  in
  let tuple_expand (x, y) = (_expand_x x, _expand_y y) in
  let bottom_left, bottom_right = bounds walls in
  let tuple_compress (x, y) = (_compress_x x, _compress_y y) in
  let (xmin, ymin), (xmax, ymax) =
    (tuple_compress bottom_left, tuple_compress bottom_right)
  in
  Queue.enqueue queue start;
  Hashtbl.add_exn distance ~key:start ~data:0;
  while (not (Queue.is_empty queue)) && not !loop_done do
    let next = Queue.dequeue_exn queue in
    Stdio.printf "pop loop %s (original %s)\n" (show_tuple next)
      (show_tuple @@ tuple_expand next);
    Stdio.printf "distance %d\n" (Hashtbl.find_exn distance next);
    if equal_tuple next goal then (
      Stdio.printf "loop done\n";
      loop_done := true)
    else
      List.iter
        ~f:(fun neighbor ->
          let x_distance, y_distance =
            match (neighbor, next) with
            | (x1, y1), (x2, y2) ->
                ( Int.abs (_expand_x x2 - _expand_x x1),
                  Int.abs (_expand_y y2 - _expand_y y1) )
          in
          Stdio.printf
            ">>> neighbor is %s! (original %s) x-distance %d y-distance %d <<<\n"
            (show_tuple neighbor)
            (show_tuple @@ tuple_expand neighbor)
            x_distance y_distance;
          if not (Hash_set.mem explored neighbor) then (
            Hash_set.add explored neighbor;
            Hashtbl.set distance ~key:neighbor
              ~data:(x_distance + y_distance + Hashtbl.find_exn distance next);
            Queue.enqueue queue neighbor))
        (cardinal_deltas
        |> List.map ~f:(fun (dx, dy) -> (fst next + dx, snd next + dy))
        |> List.filter ~f:(fun sq -> not (Set.mem walls (tuple_expand sq)))
        |> List.filter ~f:(fun (x, y) ->
               x >= xmin && x <= xmax && y >= ymin && y <= ymax))
  done;
  Stdio.printf "bounds %s %s\n"
    (show_tuple (xmin, xmax))
    (show_tuple (ymin, ymax));
  for y = ymin to ymax do
    for x = xmin to xmax do
      Stdio.printf "%c"
        (if equal_tuple (x, y) goal then 'E'
         else if equal_tuple start (x, y) then 'S'
         else if Set.mem walls (tuple_expand (x, y)) then '#'
         else if Hashtbl.mem distance (x, y) then '*'
         else '.')
    done;
    Stdio.printf "\n"
  done;
  (* well so the distances are all wrong *)
  Stdio.printf "done with loop, of course we have found the goal??? (goal %s)\n"
    (show_tuple goal);
  Hashtbl.find_exn distance goal

let _ = flood_fill_compressed

let part3 s =
  let sq, walls =
    parse_instructions s
    |> build_walls (0, 0, North, Set.empty (module IntPair_Comparator))
  in
  let _invert = map_invert_exn (module Int) in
  let x_compressor, y_compressor =
    (compression fst walls, compression snd walls)
  in
  let x_biggener, y_biggener = (_invert x_compressor, _invert y_compressor) in
  let start = (Map.find_exn x_compressor 0, Map.find_exn y_compressor 0) in
  let goal =
    (Map.find_exn x_compressor (fst sq), Map.find_exn y_compressor (snd sq))
  in
  Stdio.printf "(0,0) = %s; %s = %s\n" (show_tuple start) (show_tuple sq)
    (show_tuple goal);
  flood_fill_compressed start goal walls
    (x_compressor, y_compressor, x_biggener, y_biggener)

let%test_unit "part3 (given for part1)" =
  [%test_eq: int] 16
    (part3 "L6,L3,L6,R3,L6,L3,L3,R6,L6,R6,L6,L6,R3,L3,L3,R3,R3,L6,L6,L3")
