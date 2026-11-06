open Base
open Util

let example =
  [
    "....1.9.6.1.7....";
    "...9.2.3.3.8.1...";
    "..7.8.3.5.1.4.3..";
    ".4.6.7.4.1.1.8.9.";
    "4.5.3.6.5.8.9.4.9";
    ".9.5.1.5.6.2.8.9.";
    "..7.7.6.4.6.2.4..";
    "...9.3.8.3.8.4...";
    "....1.6.6.2.8....";
  ]

let parse_lines l =
  let g = to_grid l in
  ( g.bounds,
    grid_foldi
      ~f:(fun coord m ch ->
        if equal_char ch '.' then m
        else Map.add_exn m ~key:coord ~data:(Some (char_to_int ch)))
      ~init:(Map.empty (module IntPair_Comparator))
      g )

let trace_shadow ((xmax, ymax), m) (dx, dy) (x, y) =
  let rec ray (x, y) shadows result =
    (* if OOB, we're done *)
    if x > xmax || y > ymax || x < -1 || y < -1 then result
    else
      match Map.find m (x, y) with
      (* nothing there, shadows don't decrease I guess *)
      | None -> ray (x + dx, y + dy) shadows result
      | Some p -> (
          (* must be > all shadows *)
          (* note I think we can prune the shadows to just the greatest one.  test after implemented as written *)
          let next_shadows =
            List.filter_map
              ~f:(fun n -> if n > 1 then Some (n - 1) else None)
              shadows
          in
          match p with
          | None -> ray (x + dx, y + dy) next_shadows result
          | Some n ->
              if (not (equal n 0)) && List.for_all shadows ~f:(fun s -> n > s)
              then
                ray (x + dx, y + dy) ((n - 1) :: next_shadows) ((x, y) :: result)
              else ray (x + dx, y + dy) next_shadows result)
  in
  ray (x, y) [] []

let starting_coords (xmax, ymax) =
  (* we're just going to do the entire border, buffering it by emptiness *)
  List.concat
    [
      List.map ~f:(fun x -> (x, -1)) (List.range (-1) ~stop:`inclusive xmax);
      List.map ~f:(fun x -> (x, ymax)) (List.range (-1) ~stop:`inclusive xmax);
      List.map ~f:(fun y -> (-1, y)) (List.range 0 ymax);
      List.map ~f:(fun y -> (xmax, y)) (List.range 0 ymax);
    ]

let part1 l =
  let parsed = parse_lines l in
  starting_coords (fst parsed)
  |> List.map ~f:(trace_shadow parsed (1, 1))
  |> List.concat |> List.length

let%test_unit "part1" = [%test_eq: int] 32 (part1 example)

let _print_result (xmax, ymax) m r =
  let s = Set.of_list (module IntPair_Comparator) r in
  for y = 0 to ymax do
    for x = 0 to xmax do
      if Set.mem s (x, y) then
        match Map.find_exn m (x, y) with
        | None -> Stdio.printf "X"
        | Some n -> Stdio.printf "%d" n
      else Stdio.printf "."
    done;
    Stdio.printf "\n"
  done;
  Stdio.printf "\n"

let part2 l =
  let parsed = parse_lines l in
  [ (1, 1); (-1, 1); (-1, 0); (-1, -1); (1, -1); (1, 0) ]
  |> List.map ~f:(fun delta ->
      starting_coords (fst parsed)
      |> List.map ~f:(trace_shadow parsed delta)
      |> List.concat |> List.length)
  |> List.sum (module Int) ~f:Fn.id

let%test_unit "part2" = [%test_eq: int] 187 (part2 example)

(* must handle ents receiving energy, leaving at 9, becoming seeds and growing for 3 rounds, then joining at height 1, etc etc *)

let step_seeds =
  Map.fold
    ~init:
      ( Map.empty (module IntPair_Comparator),
        Map.empty (module IntPair_Comparator) )
    ~f:(fun ~key:coords ~data:num_left (acc_m, new_ents) ->
      match num_left with
      | 0 -> (acc_m, Map.add_exn new_ents ~key:coords ~data:(Some 1))
      | n -> (Map.add_exn acc_m ~key:coords ~data:(n - 1), new_ents))

(* convert map into new map + new seeds *)
let step_results m =
  List.fold
    ~init:(m, Map.empty (module IntPair_Comparator))
    ~f:(fun (m, seeds) coords ->
      match Map.find_exn m coords with
      | None -> (m, seeds)
      | Some 9 ->
          ( Map.set m ~key:coords ~data:None,
            Map.add_exn seeds ~key:coords ~data:3 )
      | Some n -> (Map.set m ~key:coords ~data:(Some (n + 1)), seeds))

let part3 l =
  let seq = [| (1, 1); (-1, 1); (-1, 0); (-1, -1); (1, -1); (1, 0) |] in
  let bounds, m = parse_lines l in
  let starts = starting_coords bounds in
  let step (m, seeds, i) =
    let delta = seq.(i % 6) in
    let results =
      starts |> List.map ~f:(trace_shadow (bounds, m) delta) |> List.concat
    in
    Stdio.printf "----- %d -----\n\n" (List.length results);
    _print_result bounds m results;
    let next_m, new_seeds = step_results m results in
    let next_seeds, new_ents = step_seeds seeds in
    Some
      ( (Map.length new_seeds, List.length results),
        ( Map.merge_skewed next_m new_ents ~combine:(fun ~key:_ v1 v2 ->
              match (v1, v2) with
              (* new_ents will be 1 always, we will always be taking over an
                 existing spot *)
              | None, Some 1 -> Some 1
              | _ -> failwith "impossible"),
          Map.merge_skewed new_seeds next_seeds ~combine:(fun ~key:_ -> max),
          i + 1 ) )
  in
  Sequence.unfold ~init:(m, Map.empty (module IntPair_Comparator), 0) ~f:step
  |> Sequence.fold_until ~init:(0, 0)
       ~f:(fun (total_warriors_seen, total_energy) (new_warriors, energy) ->
         Stdio.printf "(%d %d)\n" new_warriors energy;
         if total_warriors_seen + new_warriors >= 100000 then
           Stop (total_energy + energy)
         else
           Continue (total_warriors_seen + new_warriors, total_energy + energy))
       ~finish:(fun _ -> failwith "impossible")

let%test_unit "part3" = [%test_eq: int] 899889 (part3 example)
