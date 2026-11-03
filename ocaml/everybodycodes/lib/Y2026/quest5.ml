open Base
open Util

let example =
  [
    "010100100100001101000";
    "111101001000001110000";
    "101111011000101010011";
    "001000101010000100000";
    "000110100110011001100";
    "000001110111111000000";
    "101100010101011110000";
    "001101100000001010011";
    "001001001111010001001";
    "100011011110011010101";
    "001100101110010010000";
  ]

let%test_unit "i remember how my grid code works" =
  [%test_eq: char] '0' @@ grid_at (to_grid example) (0, 0)

let initial_dark_cells grid =
  grid_foldi grid
    ~init:(Set.empty (module IntPair_Comparator))
    ~f:(fun coord acc ch ->
      if equal_char ch '1' then Set.add acc coord else acc)

let center grid =
  let height, width = grid.bounds in
  ((height - 1) / 2, (width - 1) / 2)

let initial_land_cells grid =
  Set.singleton (module IntPair_Comparator) (center grid)

let initial_frontier grid =
  Set.of_list
    (module IntPair_Comparator)
    (grid_all_neighbors grid (center grid))

let step grid dark_cells (land_cells, frontier) =
  (* for every item in the frontier, check if it borders 5 dark cells + land cells
     if so, mark it as land (remove it from the frontier), and add all its
      neighbors to the frontier

      yield the current number of land cells (total)
  *)
  let becomes_land neighbor =
    Set.mem dark_cells neighbor || Set.mem land_cells neighbor
  in
  let new_land =
    Set.filter frontier ~f:(fun coord ->
        List.count (grid_all_neighbors grid coord) ~f:becomes_land >= 5)
  in
  if Set.is_empty new_land then None
  else
    let next_land = Set.union land_cells new_land in
    let new_neighbors =
      Set.fold
        ~init:(Set.empty (module IntPair_Comparator))
        ~f:(fun acc coord ->
          List.fold ~init:acc ~f:Set.add (grid_all_neighbors grid coord))
        new_land
    in
    let next_frontier = Set.diff (Set.union frontier new_neighbors) next_land in
    Some (Set.length next_land, (next_land, next_frontier))

let land_seq grid =
  Sequence.unfold
    ~init:(initial_land_cells grid, initial_frontier grid)
    ~f:(step grid (initial_dark_cells grid))

let part1 l =
  let grid = to_grid l in
  List.hd_exn @@ Sequence.to_list_rev (land_seq grid)

let%test_unit "part1" = [%test_eq: int] 137 (part1 example)
let part2 _ = 1
let part3 _ = 1
