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
  if not (equal_char (grid_at triangle.grid coords) 'T') then false
  else if not (equal_char (grid_at triangle.grid ncoords) 'T') then false
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
