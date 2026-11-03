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

type status = Landing | Sanding

let initial_sand grid land_cells =
  Set.filter land_cells ~f:(fun (x, y) ->
      (* any neighbor not in the land cells *)
      match
        List.find
          ~f:(Fn.non (Set.mem land_cells))
          (grid_all_neighbors grid (x, y))
      with
      | Some _ -> true
      (* or on the edge of the map *)
      | None ->
          equal x 0 || equal y 0
          || equal x (fst grid.bounds - 1)
          || equal y (snd grid.bounds - 1))

let set_neighbors grid set =
  Set.fold
    ~init:(Set.empty (module IntPair_Comparator))
    ~f:(fun acc coord ->
      List.fold ~init:acc ~f:Set.add (grid_all_neighbors grid coord))
    set

let step grid dark_cells (s, land_cells, sand_cells, frontier) =
  match s with
  | Landing ->
      (* for every item in the frontier, check if it borders 5 dark cells + land cells
     if so, mark it as land (remove it from the frontier), and add all its
      neighbors to the frontier

      yield the current number of land cells (total)
  *)
      let land_adjacent neighbor =
        Set.mem dark_cells neighbor || Set.mem land_cells neighbor
      in
      let new_land =
        Set.filter frontier ~f:(fun coord ->
            List.count (grid_all_neighbors grid coord) ~f:land_adjacent >= 5)
      in
      if Set.is_empty new_land then
        let next_sand = initial_sand grid land_cells in
        let next_frontier =
          Set.inter (set_neighbors grid next_sand) land_cells
        in
        let next_land = Set.diff land_cells next_sand in
        Some
          ( (Set.length next_land, Set.length next_sand),
            (Sanding, Set.diff land_cells next_sand, next_sand, next_frontier)
          )
      else
        let next_land = Set.union land_cells new_land in
        let new_neighbors = set_neighbors grid new_land in
        let next_frontier =
          Set.diff (Set.union frontier new_neighbors) next_land
        in
        Some
          ( (Set.length next_land, Set.length sand_cells),
            (Landing, next_land, sand_cells, next_frontier) )
  | Sanding ->
      let sand_adjacent neighbor = Set.mem sand_cells neighbor in
      let new_sand =
        Set.filter frontier ~f:(fun coord ->
            List.count (grid_all_neighbors grid coord) ~f:sand_adjacent >= 5)
      in
      if Set.is_empty new_sand then None
      else
        let next_land = Set.diff land_cells new_sand in
        let next_sand = Set.union sand_cells new_sand in
        let next_frontier =
          Set.diff (Set.union frontier (set_neighbors grid new_sand)) next_sand
        in
        Some
          ( (Set.length next_land, Set.length next_sand),
            (Sanding, next_land, next_sand, next_frontier) )

let land_seq grid =
  Sequence.unfold
    ~init:
      ( Landing,
        initial_land_cells grid,
        Set.empty (module IntPair_Comparator),
        initial_frontier grid )
    ~f:(step grid (initial_dark_cells grid))

let part1 l =
  let grid = to_grid l in
  let l, s =
    Sequence.find_exn (land_seq grid) ~f:(fun (_, s) -> not (equal s 0))
  in
  l + s

let%test_unit "part1" = [%test_eq: int] 137 (part1 example)

let part2 l =
  let grid = to_grid l in
  snd @@ List.hd_exn @@ Sequence.to_list_rev
  @@ Sequence.take (land_seq grid) 1000 (* just clamp this *)

let%test_unit "part2" = [%test_eq: int] 69 (part2 example)
let part3 _ = 1
