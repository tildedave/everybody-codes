open Base
open Util
open Search

let part1 l =
  let g = to_grid l in
  let xmax, ymax = g.bounds in
  dijkstra
    (module IntPair)
    (0, 0)
    ~is_goal:(fun (_, coord) -> equal_tuple (xmax - 1, ymax - 1) coord)
    ~neighbors:(fun coord ->
      Sequence.of_list
      @@ List.filter
           ~f:(fun neighbor -> equal_char '#' (grid_at g neighbor))
           (grid_cardinal_neighbors g coord))
    ~should_cutoff:(fun _ -> false)
  |> fst |> Option.value_exn

let example =
  [
    "####.######";
    "#.#.##..#..";
    "##.##.##..#";
    "#.#.###.###";
    "#.###.###.#";
    "###.##.#.##";
  ]

let%test_unit "part1" = [%test_result: int] ~expect:21 (part1 example)

let part2 l =
  let g = to_grid l in


let part2 _ = 1
let part3 _ = 1
