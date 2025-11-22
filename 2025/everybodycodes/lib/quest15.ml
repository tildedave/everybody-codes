open Base
open Util

type orientation = North | South | East | West
type direction = Left of int | Right of int

let parse_instructions s =
  s |> String.split ~on:','
  |> List.map ~f:(fun s ->
         let n = num_of_char s.[1] in
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
  | [] -> ((x, y), walls)
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

let flood_fill (sq, walls) =
  let queue, explored, distance, loop_done =
    ( Queue.create (),
      Hash_set.create (module IntPair),
      Hashtbl.create (module IntPair),
      ref false )
  in
  Queue.enqueue queue sq;
  Hashtbl.add_exn distance ~key:sq ~data:0;
  while (not (Queue.is_empty queue)) && not !loop_done do
    let next = Queue.dequeue_exn queue in
    if equal_tuple next (0, 0) then loop_done := true
    else
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem explored neighbor) then (
            Hash_set.add explored next;
            Hashtbl.set distance ~key:neighbor
              ~data:(1 + Hashtbl.find_exn distance next);
            Queue.enqueue queue neighbor))
        (cardinal_deltas
        |> List.map ~f:(fun (dx, dy) -> (fst next + dx, snd next + dy))
        |> List.filter ~f:(fun sq -> not (Set.mem walls sq)))
  done;
  Hashtbl.find_exn distance (0, 0)

let part1 s =
  parse_instructions s
  |> follow_directions (0, 0, North, Set.empty (module IntPair_Comparator))
  |> flood_fill

let%test_unit "part1 (given 1)" =
  [%test_eq: int] 6 (part1 "R3,R4,L3,L4,R3,R6,R9")

let%test_unit "part1 (given 2)" =
  [%test_eq: int] 16
    (part1 "L6,L3,L6,R3,L6,L3,L3,R6,L6,R6,L6,L6,R3,L3,L3,R3,R3,L6,L6,L3")

let part2 _ = 1
let part3 _ = 1
