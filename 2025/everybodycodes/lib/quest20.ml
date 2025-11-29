open Base
open Util

type polarity = Up | Down [@@deriving eq, show]

let _ = pp_polarity

type triangle = {
  grid : grid;
  polarity :
    ( IntPair_Comparator.t,
      polarity,
      IntPair_Comparator.comparator_witness )
    Map.t;
}

let to_triangle grid =
  let open Array in
  let result = ref (Map.empty (module IntPair_Comparator)) in
  for y = 0 to length grid.cells - 1 do
    let seen_beginning = ref false in
    let polarity = ref Up in
    for x = 0 to length grid.cells.(y) - 1 do
      if (not !seen_beginning) && not (equal_char grid.cells.(y).(x) '.') then
        seen_beginning := true;
      if !seen_beginning then (
        result := Map.add_exn !result ~key:(x, y) ~data:!polarity;
        if equal_polarity !polarity Up then polarity := Down else polarity := Up)
    done
  done;
  { grid; polarity = !result }

type direction = Left | Up | Down | Right [@@deriving equal, show]

let _ = show_direction
let _ = pp_direction

let direction_between (x, y) (x', y') =
  match (x' - x, y' - y) with
  | 0, 1 -> Down
  | 0, -1 -> Up
  | 1, 0 -> Right
  | -1, 0 -> Left
  | _ -> failwith "invalid"

let can_jump triangle coords ncoords =
  let from_char = grid_at triangle.grid coords in
  let to_char = grid_at triangle.grid ncoords in

  if not (equal_char from_char 'T' || equal_char from_char 'S') then false
  else if not (equal_char to_char 'T' || equal_char to_char 'E') then false
  else
    let polarity, npolarity =
      ( Map.find_exn triangle.polarity coords,
        Map.find_exn triangle.polarity ncoords )
    in
    if equal_polarity polarity npolarity then false
    else
      match (direction_between coords ncoords, polarity, npolarity) with
      | Down, Up, Down -> false
      | Up, Down, Up -> false
      | _ -> true

let part1 l =
  let t = l |> to_grid |> to_triangle in
  let result =
    grid_fold ~init:0
      ~f:(fun coords acc _ ->
        List.fold ~init:acc
          ~f:(fun acc ncoords ->
            if can_jump t coords ncoords then acc + 1 else acc)
          (grid_cardinal_neighbors t.grid coords))
      t.grid
  in
  result / 2

(* BFS is fine I guess *)
let part2 l =
  let t = l |> to_grid |> to_triangle in
  let queue = Queue.create () in
  let start = t.grid |> grid_find ~f:(equal_char 'S') |> Option.value_exn in
  let goal = t.grid |> grid_find ~f:(equal_char 'E') |> Option.value_exn in
  let distance = Hashtbl.create (module IntPair) in
  let explored = Hash_set.create (module IntPair) in
  Queue.enqueue queue start;
  Hashtbl.set distance ~key:start ~data:0;
  Hash_set.add explored start;
  let loop_done = ref false in
  while (not (Queue.is_empty queue)) && not !loop_done do
    let next = Queue.dequeue_exn queue in
    if equal_tuple next goal then loop_done := true
    else
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem explored neighbor) then (
            Hash_set.add explored neighbor;
            Hashtbl.set distance ~key:neighbor
              ~data:(Hashtbl.find_exn distance next + 1);
            Queue.enqueue queue neighbor))
        (List.filter ~f:(can_jump t next) (grid_cardinal_neighbors t.grid next))
  done;
  Hashtbl.find_exn distance goal
