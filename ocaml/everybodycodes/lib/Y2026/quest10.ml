[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]

open Base
open Util

let example =
  [
    "###########";
    "#####.#####";
    "###.....###";
    "##.......##";
    "##.>>.<<.##";
    "#v...>...v#";
    "##...v...##";
    "##.......##";
    "###.....###";
    "#####.#####";
    "###########";
  ]

type ghost = {
  id : int;
  direction : int * int;
  position : int * int;
  num_collisions : (int[@compare.ignore]);
}
[@@deriving compare, sexp_of, show]

module Ghost = struct
  type t = ghost [@@deriving sexp_of, show, compare]

  (* don't use num_collisions in position hash *)
  (* let hash g = Hashtbl.hash (g.id, g.direction, g.position) *)
end

module GhostComparator = struct
  include Ghost
  include Base.Comparator.Make (Ghost)
end

let _ = Set.empty (module GhostComparator)

module GhostSet = struct
  type t = Set.M(GhostComparator).t [@@deriving compare, sexp_of]
end

module GhostSetComparator = struct
  include GhostSet
  include Base.Comparator.Make (GhostSet)
end

let%test_unit "ghost comparator ignores collisions" =
  let ghost =
    { id = 1; position = (0, 0); direction = (1, 0); num_collisions = 1 }
  in
  [%test_eq: bool] true
    ((Set.mem (Set.singleton (module GhostComparator) ghost))
       { ghost with num_collisions = 2 })

let%test_unit "ghost comparator does not ignore position changes" =
  let ghost =
    { id = 1; position = (0, 0); direction = (1, 0); num_collisions = 1 }
  in
  [%test_eq: bool] false
    ((Set.mem (Set.singleton (module GhostComparator) ghost))
       { ghost with position = (1, 0) })

let id_ref = ref 0

let new_id () =
  id_ref := !id_ref + 1;
  !id_ref

let parse_lines l =
  let g = to_grid l in
  ( g,
    grid_foldi
      ~init:(Map.empty (module IntPair_Comparator))
      ~f:(fun coords acc ch ->
        match ch with
        | '>' ->
            Map.add_multi acc ~key:coords
              ~data:
                {
                  id = new_id ();
                  position = coords;
                  direction = (1, 0);
                  num_collisions = 0;
                }
        | '<' ->
            Map.add_multi acc ~key:coords
              ~data:
                {
                  id = new_id ();
                  position = coords;
                  direction = (-1, 0);
                  num_collisions = 0;
                }
        | '^' ->
            Map.add_multi acc ~key:coords
              ~data:
                {
                  id = new_id ();
                  position = coords;
                  direction = (0, -1);
                  num_collisions = 0;
                }
        | 'v' ->
            Map.add_multi acc ~key:coords
              ~data:
                {
                  id = new_id ();
                  position = coords;
                  direction = (0, 1);
                  num_collisions = 0;
                }
        | _ -> acc)
      g )

let rotate_right (dx, dy) =
  match (dx, dy) with
  | 0, -1 -> (1, 0)
  | 0, 1 -> (-1, 0)
  | 1, 0 -> (0, 1)
  | -1, 0 -> (0, -1)
  | _ -> failwith "invalid deltas"

let flip ghost ~explode_at =
  if equal (ghost.num_collisions + 1) explode_at then None
  else
    let dx, dy = ghost.direction in
    Some
      {
        ghost with
        direction = (-dx, -dy);
        num_collisions = ghost.num_collisions + 1;
      }

let _print_map g m =
  for y = 0 to snd g.bounds - 1 do
    for x = 0 to fst g.bounds - 1 do
      if equal_char (grid_at g (x, y)) '#' then Stdio.printf "#"
      else
        match Map.find m (x, y) with
        | None -> Stdio.printf "."
        | Some [ x ] ->
            Stdio.printf "%s"
              (match x.direction with
              | 0, -1 -> "^"
              | 0, 1 -> "v"
              | 1, 0 -> ">"
              | -1, 0 -> "<"
              | _ -> failwith "impossible")
        | Some l -> Stdio.printf "%d" (List.length l)
    done;
    Stdio.printf "\n"
  done;
  Stdio.printf "\n"

let move g m ~explode_at =
  (* _print_map g m; *)
  Map.fold
    ~init:(Map.empty (module IntPair_Comparator))
    ~f:(fun ~key:(x, y) ~data:ghosts acc_m ->
      List.fold ~init:acc_m
        ~f:(fun acc_m ghost ->
          let dx, dy = ghost.direction in
          let nx, ny = (x + dx, y + dy) in
          if equal_char (grid_at g (nx, ny)) '#' then
            if equal (ghost.num_collisions + 1) explode_at then acc_m
            else
              Map.add_multi acc_m ~key:(x, y)
                ~data:
                  {
                    ghost with
                    direction = rotate_right (dx, dy);
                    num_collisions = ghost.num_collisions + 1;
                  }
          else
            Map.add_multi acc_m ~key:(nx, ny)
              ~data:{ ghost with position = (nx, ny) })
        ghosts)
    m

let collide ~explode_at =
  Map.fold
    ~init:(Map.empty (module IntPair_Comparator))
    ~f:(fun ~key:coords ~data:ghosts next_m ->
      match ghosts with
      | [] -> failwith "impossible"
      | [ _ ] -> Map.set next_m ~key:coords ~data:ghosts
      | _ ->
          Map.set next_m ~key:coords
            ~data:(List.filter_map ~f:(flip ~explode_at) ghosts))

let total_collisions =
  Map.sum (module Int) ~f:(List.sum (module Int) ~f:(fun g -> g.num_collisions))

let move_seq ?(explode_at = 10000) l =
  let g, m = parse_lines l in
  Sequence.unfold ~init:m ~f:(fun m ->
      let next_m = m |> move ~explode_at g |> collide ~explode_at in
      Some (m, next_m))

let collision_seq ?(explode_at = 10000) l =
  let seq = move_seq ~explode_at l |> Sequence.map ~f:total_collisions in
  Sequence.map ~f:(fun (before, after) -> after - before)
  @@ Sequence.zip seq (Sequence.tl_eagerly_exn seq)

let part1 l =
  Sequence.sum (module Int) (Sequence.take (collision_seq l) 10) ~f:Fn.id

let%test_unit "collision_seq" =
  [%test_eq: int list]
    [ 4; 3; 1; 1; 8; 3; 1; 3; 1; 4 ]
    (Sequence.to_list (Sequence.take (collision_seq example) 10))

let%test_unit "part1" = [%test_eq: int] 29 (part1 example)
let remaining_ghosts = Map.sum (module Int) ~f:List.length

let ghost_seq ?(explode_at = 10000) l =
  move_seq ~explode_at l |> Sequence.map ~f:remaining_ghosts

let _part2 ~explode_at l =
  ghost_seq ~explode_at l
  |> Sequence.take_while ~f:(fun n -> n > 0)
  |> Sequence.length

let part2 = _part2 ~explode_at:10000
let%test_unit "part2" = [%test_eq: int] 53 (_part2 ~explode_at:10 example)

let%test_unit "part2 (other example)" =
  [%test_eq: int] 50000 (_part2 ~explode_at:10000 example)

(* part3 standard ramp up - I assume the state of the simulation repeats over and over *)
(* can do the silly thing and find the repeats *)
(* to string may not preserve direction, so we need something a little smarter for caching *)

let all_ghosts m =
  List.fold
    ~init:(Set.empty (module GhostComparator))
    (Map.data m)
    ~f:(fun acc -> List.fold ~f:Set.add ~init:acc)

let find_repeat seq =
  Sequence.fold_until
    ~init:(0, Map.empty (module GhostSetComparator))
    ~f:(fun (i, acc) m ->
      let current_ghosts = all_ghosts m in
      match Map.find acc current_ghosts with
      | Some (j, m') ->
          (* must find the original key I guess *)
          let original_ghosts =
            List.find_exn
              ~f:(fun gs -> equal (GhostSet.compare gs current_ghosts) 0)
              (Map.keys acc)
          in
          Stop (m', j, original_ghosts, i, current_ghosts)
      | None ->
          Continue (i + 1, Map.add_exn acc ~key:current_ghosts ~data:(i, m)))
    ~finish:(fun _ -> failwith "impossible")
    seq

let p3_explode = 1000000000000

let p3_example =
  [
    "#######################";
    "###########.###########";
    "#######.^>.^.<^.#######";
    "#####..<.>.v^>>^^<#####";
    "####..v<^v.>.>v..^.####";
    "###.................###";
    "###.................###";
    "##.......<...<.<....>##";
    "##...v....>....v.v<.^##";
    "##........###........##";
    "##v......#####.......##";
    "#.<......#####......>.#";
    "##.......#####.......##";
    "##........###........##";
    "##...................##";
    "##...................##";
    "###.................###";
    "###.................###";
    "####...............####";
    "#####.............#####";
    "#######....v....#######";
    "###########.###########";
    "#######################";
  ]

let _print_ghosts = Set.iter ~f:(fun g -> Stdio.printf "%s\n" (show_ghost g))

let collision_deltas original_ghosts new_ghosts =
  Set.fold
    ~init:(Map.empty (module GhostComparator))
    ~f:(fun acc g ->
      Map.add_exn acc ~key:g
        ~data:
          ((Set.find_exn new_ghosts ~f:(fun g' -> equal (compare_ghost g' g) 0))
             .num_collisions - g.num_collisions))
    original_ghosts

let num_cycles_to_explode m deltas ~explode_at =
  all_ghosts m
  |> Set.map
       (module Int)
       ~f:(fun ghost ->
         let ghost_delta = Map.find_exn deltas ghost in
         (* need ghost.num_collisions + ghost_delta * n >= explode_at  - solve for n *)
         (* n = (explode_at - ghost.num_collisions) / ghost_delta *)
         (* technically it's the upper float of this *)
         let result = (explode_at - ghost.num_collisions) / ghost_delta in
         if equal ((explode_at - ghost.num_collisions) % ghost_delta) 0 then
           result - 1
         else result)
  |> Set.min_elt |> Option.value_exn

let advance m deltas num_cycles =
  Map.map m
    ~f:
      (List.map ~f:(fun ghost ->
           let delta = Map.find_exn deltas ghost in
           {
             ghost with
             num_collisions = ghost.num_collisions + (delta * num_cycles);
           }))

let _move_seq ~explode_at g m =
  Sequence.unfold ~init:m ~f:(fun m ->
      let next_m = m |> move ~explode_at g |> collide ~explode_at in
      Some (m, next_m))

let iterated_ghost_seq ~explode_at l =
  let g, m = parse_lines l in
  Sequence.unfold ~init:(0, m) ~f:(fun (t, m) ->
      if equal (Set.length (all_ghosts m)) 0 then None
      else
        let m', di, original, dj, looped =
          find_repeat (_move_seq ~explode_at g m)
        in
        let deltas = collision_deltas original looped in
        let num_cycles = num_cycles_to_explode m' deltas ~explode_at in
        let next_m' = advance m' deltas num_cycles in
        let orig_ghost_count = Set.length (all_ghosts next_m') in
        let t_explode, m' =
          Sequence.fold_until ~init:0
            ~f:(fun i m ->
              if equal (Set.length (all_ghosts m)) orig_ghost_count then
                Continue (i + 1)
              else Stop (i, m))
            ~finish:(fun _ -> failwith "impossible")
            (_move_seq ~explode_at g next_m')
        in
        let next_t = t + di + (num_cycles * (dj - di)) + t_explode in
        Some (next_t, (next_t, m')))

(* let%test_unit "iterated_ghost_seq" =
  [%test_eq: int list]
    [
      666666666760;   -- my sequence doesn't include all these steps
      666666666796;
      999999999994;
      1000000000000;
      1000000000033;
      1000000000052;
      1000000000059;
      1000000000060;
      1000000000063;
      1000000000065;
      1000000000067;
      1000000000070;
      1000000000076;
      1000000000085;
      1000000000090;
      1000000000096;
      1000000000098;
      1000000000129;
      1000000000236;
      1444444444455;
      1444444444564;
      1571428573720;
      1716049383661;
      1740740741644;
      1740740741811;
      1833333335427;
      2208137718009;
      5381126880052;
      11286225837575;
      13314695352588;
    ]
    (Sequence.to_list (iterated_ghost_seq ~explode_at:p3_explode p3_example)) *)

let part3 l =
  List.hd_exn
    (Sequence.to_list_rev (iterated_ghost_seq ~explode_at:p3_explode l))

let%test_unit "part3 (other example)" =
  [%test_eq: int] 13314695352588 (part3 p3_example)
