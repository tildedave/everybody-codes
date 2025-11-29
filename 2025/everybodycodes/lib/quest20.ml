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

let is_above (x, y) (x', y') = x = x' && y = y' - 1

let part1 l =
  let t = l |> to_grid |> to_triangle in
  let result =
    grid_fold ~init:0
      ~f:(fun coords acc ch ->
        if equal_char ch 'T' then
          let polarity = Map.find_exn t.polarity coords in
          List.fold ~init:acc
            ~f:(fun acc ncoords ->
              if not (equal_char (grid_at t.grid ncoords) 'T') then acc
              else
                let npolarity = Map.find_exn t.polarity ncoords in
                if equal_polarity polarity npolarity then acc
                else if
                  is_above coords ncoords
                  &&
                  match (polarity, npolarity) with
                  | Up, Down -> true
                  | _ -> false
                then acc
                else acc + 1)
            (grid_neighbors [ (0, 1); (1, 0) ] t.grid coords)
        else acc)
      t.grid
  in
  result
