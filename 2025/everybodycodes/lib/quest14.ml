open Base
open Util

(* game of life *)

let next_state grid state =
  grid_all_coords grid
  |> List.fold
       ~f:(fun next_state c ->
         let active_neighbor_count =
           grid_diagonal_neighbors grid c
           |> List.filter ~f:(fun n -> Set.mem state n)
           |> List.length
         in
         if Set.mem state c && active_neighbor_count % 2 = 1 then
           Set.add next_state c
         else if (not (Set.mem state c)) && active_neighbor_count % 2 = 0 then
           Set.add next_state c
         else next_state)
       ~init:(Set.empty (module IntPair_Comparator))

let initial_coords grid =
  grid_fold grid
    ~f:(fun c acc ch -> if equal_char ch '#' then Set.add acc c else acc)
    ~init:(Set.empty (module IntPair_Comparator))

let count_active_tiles num_rounds l =
  let grid = to_grid l in
  let rec loop state i count =
    let next = next_state grid state in
    if i = num_rounds then count else loop next (i + 1) (count + Set.length next)
  in
  loop (initial_coords grid) 0 0

let part1 = count_active_tiles 10

let%test_unit "part1 (given)" =
  [%test_eq: int] 200
    (part1 [ ".#.##."; "##..#."; "..##.#"; ".#.##."; ".###.."; "###.##" ])

let part2 = count_active_tiles 2025

(* 34x34, start from blank state, look for goal state, count active tiles if it matches *)
(* 1 billion may still be computable :-) *)

let build_pattern_sets grid =
  grid_fold grid
    ~f:(fun (x, y) (ps, aps) ch ->
      if equal_char ch '#' then (Set.add ps (x + 13, y + 13), aps)
      else (ps, Set.add aps (x + 13, y + 13)))
    ~init:
      ( Set.empty (module IntPair_Comparator),
        Set.empty (module IntPair_Comparator) )

let compute_pattern hits =
  if List.length hits = 1 then None
  else
    match (List.hd_exn hits, List.last_exn hits) with
    | (r1, added, total), (r1', added', total') ->
        if added = added' then Some (r1' - r1, total' - total) else None

let part3 l =
  let dest = 1000000000 in
  let grid =
    {
      cells = Array.init 0 ~f:(fun _ -> Array.init 0 ~f:(fun _ -> ' '));
      bounds = (34, 34);
    }
  in
  let pattern_set, antipattern_set = build_pattern_sets (to_grid l) in
  let rec loop state i count hits found =
    let next = next_state grid state in
    if i = dest then count
    else if
      Set.is_subset pattern_set ~of_:next
      && Set.is_empty (Set.inter antipattern_set next)
    then (
      let next_hits =
        (i + 1, Set.length next, count + Set.length next) :: hits
      in
      Stdio.printf "[%d] pattern match!  adding %d, total now %d\n%!" (i + 1)
        (Set.length next)
        (count + Set.length next);
      if found then loop next (i + 1) (count + Set.length next) next_hits found
      else
        match compute_pattern next_hits with
        | None -> loop next (i + 1) (count + Set.length next) next_hits found
        | Some (i_delta, active_delta) ->
            loop next
              (i + ((dest - (i + 1)) / i_delta * i_delta))
              (count + Set.length next
              + ((dest - (i + 1)) / i_delta * active_delta))
              [] true)
    else loop next (i + 1) count hits found
  in
  loop (initial_coords grid) 0 0 [] false
