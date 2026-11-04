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

type status = Landing | Sanding | Planting | Growing of int

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

let can_stamp (land_cells, tree_cells) (x, y) =
  let start_x, end_x, start_y, end_y = (x - 2, x + 2, y - 2, y + 2) in
  let result =
    List.concat_map
      ~f:(fun x ->
        List.map
          ~f:(fun y -> (x, y))
          (List.range ~start:`inclusive ~stop:`inclusive start_y end_y))
      (List.range ~start:`inclusive ~stop:`inclusive start_x end_x)
    |> List.for_all ~f:(fun (x', y') ->
        (equal x' start_x && equal y' start_y)
        || (equal x' start_x && equal y' end_y)
        || (equal x' end_x && equal y' start_y)
        || (equal x' end_x && equal y' end_y)
        || (equal x' x && equal y' y)
        || Set.mem land_cells (x', y')
        || Set.mem tree_cells (x', y'))
  in
  result

let can_grow grid (sand_cells, tree_cells) coord =
  let neighbors = grid_all_neighbors grid coord in
  let num_tree_neighbors = List.count ~f:(Set.mem tree_cells) neighbors in
  let num_sand_neighbors = List.count ~f:(Set.mem sand_cells) neighbors in
  num_tree_neighbors >= 4 && equal num_sand_neighbors 0

let step grid dark_cells state =
  let s, land_cells, sand_cells, tree_cells, frontier = state in
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
          ( (Set.length next_land, Set.length next_sand, Set.length tree_cells),
            ( Sanding,
              Set.diff land_cells next_sand,
              next_sand,
              tree_cells,
              next_frontier ) )
      else
        let next_land = Set.union land_cells new_land in
        let new_neighbors = set_neighbors grid new_land in
        let next_frontier =
          Set.diff (Set.union frontier new_neighbors) next_land
        in
        Some
          ( (Set.length next_land, Set.length sand_cells, Set.length tree_cells),
            (Landing, next_land, sand_cells, tree_cells, next_frontier) )
  | Sanding ->
      let sand_adjacent neighbor = Set.mem sand_cells neighbor in
      let new_sand =
        Set.filter frontier ~f:(fun coord ->
            List.count (grid_all_neighbors grid coord) ~f:sand_adjacent >= 5)
      in
      if Set.is_empty new_sand then
        Some
          ( (Set.length land_cells, Set.length sand_cells, Set.length tree_cells),
            (Planting, land_cells, sand_cells, tree_cells, frontier) )
      else
        let next_land = Set.diff land_cells new_sand in
        let next_sand = Set.union sand_cells new_sand in
        let next_frontier =
          Set.diff (Set.union frontier (set_neighbors grid new_sand)) next_sand
        in
        Some
          ( (Set.length next_land, Set.length next_sand, Set.length tree_cells),
            (Sanding, next_land, next_sand, tree_cells, next_frontier) )
  | Planting ->
      let next_trees =
        Set.filter ~f:(can_stamp (land_cells, tree_cells)) land_cells
      in
      let next_land = Set.diff land_cells next_trees in
      Some
        ( (Set.length next_land, Set.length sand_cells, Set.length next_trees),
          (Growing 0, next_land, sand_cells, next_trees, frontier) )
  | Growing n ->
      if equal n 2 then None
      else
        let new_trees =
          Set.filter ~f:(can_grow grid (sand_cells, tree_cells)) land_cells
        in
        let next_land = Set.diff land_cells new_trees in
        let next_trees = Set.union new_trees tree_cells in
        Some
          ( (Set.length next_land, Set.length sand_cells, Set.length next_trees),
            (Growing (n + 1), next_land, sand_cells, next_trees, frontier) )

let land_seq grid =
  Sequence.unfold
    ~init:
      ( Landing,
        initial_land_cells grid,
        Set.empty (module IntPair_Comparator),
        Set.empty (module IntPair_Comparator),
        initial_frontier grid )
    ~f:(step grid (initial_dark_cells grid))

let part1 l =
  let grid = to_grid l in
  let l, s, _ =
    Sequence.find_exn (land_seq grid) ~f:(fun (_, s, _) -> not (equal s 0))
  in
  l + s

let%test_unit "part1" = [%test_eq: int] 137 (part1 example)

let part2 l =
  let grid = to_grid l in
  let _, s, _ =
    Sequence.find_exn (land_seq grid) ~f:(fun (_, _, t) -> not (equal t 0))
  in
  s

let%test_unit "part2" = [%test_eq: int] 69 (part2 example)

let part3 l =
  let grid = to_grid l in
  let _, _, t =
    List.hd_exn @@ Sequence.to_list_rev
    @@ Sequence.take (land_seq grid) 1000 (* just clamp this *)
  in
  t

let%test_unit "part3" = [%test_eq: int] 14 (part3 example)
