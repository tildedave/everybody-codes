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

type ghost = { id : int; direction : int * int; position : int * int }
[@@deriving compare, sexp_of, show]

module Ghost = struct
  type t = ghost [@@deriving sexp_of, show, compare]
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

(* let%test_unit "ghost comparator ignores collisions" =
  let ghost =
    { id = 1; position = (0, 0); direction = (1, 0); num_collisions = 1 }
  in
  [%test_eq: bool] true
    ((Set.mem (Set.singleton (module GhostComparator) ghost))
       { ghost with num_collisions = 2 }) *)

(* let%test_unit "ghost comparator does not ignore position changes" =
  let ghost =
    { id = 1; position = (0, 0); direction = (1, 0); num_collisions = 1 }
  in
  [%test_eq: bool] false
    ((Set.mem (Set.singleton (module GhostComparator) ghost))
       { ghost with position = (1, 0) }) *)

let id_ref = ref 0

let new_id () =
  id_ref := !id_ref + 1;
  !id_ref

type 'a state = {
  positions : (int * int, ghost list, 'a) Map.t;
  (* time : int; *)
  ghosts : (int, Int.comparator_witness) Set.t;
  collisions : (int, int, Int.comparator_witness) Map.t;
}

let all_ghosts m =
  List.fold
    ~init:(Set.empty (module GhostComparator))
    (Map.data m)
    ~f:(fun acc -> List.fold ~f:Set.add ~init:acc)

let parse_lines l =
  let g = to_grid l in
  ( g,
    let positions =
      grid_foldi
        ~init:(Map.empty (module IntPair_Comparator))
        ~f:(fun coords acc ch ->
          match ch with
          | '>' ->
              Map.add_multi acc ~key:coords
                ~data:{ id = new_id (); position = coords; direction = (1, 0) }
          | '<' ->
              Map.add_multi acc ~key:coords
                ~data:{ id = new_id (); position = coords; direction = (-1, 0) }
          | '^' ->
              Map.add_multi acc ~key:coords
                ~data:{ id = new_id (); position = coords; direction = (0, -1) }
          | 'v' ->
              Map.add_multi acc ~key:coords
                ~data:{ id = new_id (); position = coords; direction = (0, 1) }
          | _ -> acc)
        g
    in
    let ghosts = all_ghosts positions in
    {
      positions;
      (* time = 0; *)
      ghosts = Set.map (module Int) ~f:(fun g -> g.id) ghosts;
      collisions =
        Set.fold
          ~init:(Map.empty (module Int))
          ~f:(fun acc ghost -> Map.add_exn acc ~key:ghost.id ~data:0)
          ghosts;
    } )

let rotate_right (dx, dy) =
  match (dx, dy) with
  | 0, -1 -> (1, 0)
  | 0, 1 -> (-1, 0)
  | 1, 0 -> (0, 1)
  | -1, 0 -> (0, -1)
  | _ -> failwith "invalid deltas"

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

let move g state ~explode_at =
  (* _print_map g state.positions; *)
  Map.fold
    ~init:{ state with positions = Map.empty (module IntPair_Comparator) }
    ~f:(fun ~key:(x, y) ~data:ghosts state ->
      List.fold ~init:state
        ~f:(fun state ghost ->
          let dx, dy = ghost.direction in
          let nx, ny = (x + dx, y + dy) in
          let num_collisions = Map.find_exn state.collisions ghost.id in
          if equal_char (grid_at g (nx, ny)) '#' then
            if equal (num_collisions + 1) explode_at then
              {
                state with
                ghosts = Set.remove state.ghosts ghost.id;
                collisions = Map.remove state.collisions ghost.id;
              }
            else
              {
                state with
                positions =
                  Map.add_multi state.positions ~key:(x, y)
                    ~data:{ ghost with direction = rotate_right (dx, dy) };
                collisions =
                  Map.set state.collisions ~key:ghost.id
                    ~data:(num_collisions + 1);
              }
          else
            {
              state with
              positions =
                Map.add_multi state.positions ~key:(nx, ny)
                  ~data:{ ghost with position = (nx, ny) };
            })
        ghosts)
    state.positions

let collide state ~explode_at =
  Map.fold
    ~init:{ state with positions = Map.empty (module IntPair_Comparator) }
    ~f:(fun ~key:coords ~data:ghosts state ->
      match ghosts with
      | [] -> failwith "impossible"
      | [ _ ] ->
          {
            state with
            positions = Map.set state.positions ~key:coords ~data:ghosts;
          }
      | _ ->
          List.fold ~init:state
            ~f:(fun state ghost ->
              let num_collisions = Map.find_exn state.collisions ghost.id in
              if equal (num_collisions + 1) explode_at then
                {
                  state with
                  ghosts = Set.remove state.ghosts ghost.id;
                  collisions = Map.remove state.collisions ghost.id;
                }
              else
                let dx, dy = ghost.direction in
                {
                  state with
                  positions =
                    Map.add_multi state.positions ~key:coords
                      ~data:{ ghost with direction = (-dx, -dy) };
                  collisions =
                    Map.set state.collisions ~key:ghost.id
                      ~data:(num_collisions + 1);
                })
            ghosts)
    state.positions

let total_collisions state = Map.sum (module Int) ~f:Fn.id state.collisions

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
let remaining_ghosts state = Set.length state.ghosts

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

let find_repeat seq =
  Sequence.fold_until
    ~init:(0, Map.empty (module GhostSetComparator))
    ~f:(fun (j, acc) new_state ->
      let current_ghosts =
        Set.of_list (module GhostComparator)
        @@ List.concat (Map.data new_state.positions)
      in
      match Map.find acc current_ghosts with
      | Some (i, original_state) ->
          (* must find the original key I guess *)
          let original_ghosts =
            List.find_exn
              ~f:(fun gs -> equal (GhostSet.compare gs current_ghosts) 0)
              (Map.keys acc)
          in
          Stop (original_state, i, new_state, j)
      | None ->
          Continue
            (j + 1, Map.add_exn acc ~key:current_ghosts ~data:(j, new_state)))
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

let collision_deltas original_state new_state =
  Set.fold
    ~init:(Map.empty (module Int))
    ~f:(fun acc ghost_id ->
      Map.add_exn acc ~key:ghost_id
        ~data:
          (Map.find_exn new_state.collisions ghost_id
          - Map.find_exn original_state.collisions ghost_id))
    original_state.ghosts

let num_cycles_to_explode state deltas ~explode_at =
  state.ghosts
  |> Set.map
       (module Int)
       ~f:(fun ghost_id ->
         let ghost_delta = Map.find_exn deltas ghost_id in
         let num_collisions = Map.find_exn state.collisions ghost_id in
         (* need ghost.num_collisions + ghost_delta * n >= explode_at  - solve for n *)
         (* n = (explode_at - ghost.num_collisions) / ghost_delta *)
         (* technically it's the upper float of this *)
         let result = (explode_at - num_collisions) / ghost_delta in
         if equal ((explode_at - num_collisions) % ghost_delta) 0 then
           result - 1
         else result)
  |> Set.min_elt |> Option.value_exn

let advance state deltas num_cycles =
  {
    state with
    collisions =
      Map.mapi state.collisions ~f:(fun ~key:ghost_id ~data:v ->
          let delta = Map.find_exn deltas ghost_id in
          v + (delta * num_cycles));
  }

let _move_seq ~explode_at g m =
  Sequence.unfold ~init:m ~f:(fun m ->
      let next_m = m |> move ~explode_at g |> collide ~explode_at in
      Some (m, next_m))

let advance_to_next_explosion g ~explode_at state =
  let orig_ghost_count = remaining_ghosts state in
  Sequence.fold_until ~init:0
    ~f:(fun i state ->
      if equal (remaining_ghosts state) orig_ghost_count then Continue (i + 1)
      else Stop (i, state))
    ~finish:(fun _ -> failwith "impossible")
    (_move_seq ~explode_at g state)

let iterated_ghost_seq ~explode_at l =
  let g, state = parse_lines l in
  Sequence.unfold ~init:(0, state) ~f:(fun (t, initial_state) ->
      if equal (remaining_ghosts initial_state) 0 then None
      else
        let state, di, new_state, dj =
          find_repeat (_move_seq ~explode_at g initial_state)
        in
        if
          remaining_ghosts initial_state > remaining_ghosts state
          || remaining_ghosts state > remaining_ghosts new_state
        then (
          Stdio.printf "[%d] no advance - just forward to next explosion\n" t;
          Out_channel.flush Stdio.stdout;
          Stdio.printf "ghosts before %s\n"
            (String.concat ~sep:","
               (List.map ~f:Int.to_string (Set.to_list initial_state.ghosts)));
          let t_explode, state' =
            advance_to_next_explosion g ~explode_at initial_state
          in
          Stdio.printf "ghosts after %s\n"
            (String.concat ~sep:","
               (List.map ~f:Int.to_string (Set.to_list state'.ghosts)));
          let next_t = t + t_explode in
          Some (next_t, (next_t, state')))
        else (
          Stdio.printf "[%d] found repeat %d -> %d\n" t di dj;
          Out_channel.flush Stdio.stdout;
          Stdio.printf "ghosts at repeat start %s\n"
            (String.concat ~sep:","
               (List.map ~f:Int.to_string (Set.to_list state.ghosts)));
          Stdio.printf "ghosts at repeat end   %s\n"
            (String.concat ~sep:","
               (List.map ~f:Int.to_string (Set.to_list new_state.ghosts)));
          let deltas = collision_deltas state new_state in
          let num_cycles = num_cycles_to_explode state deltas ~explode_at in
          Stdio.printf "deltas   %s\n"
            (String.concat ~sep:", "
               (List.map
                  ~f:(fun (gid, d) -> Printf.sprintf "%d -> %d" gid d)
                  (Map.to_alist deltas)));
          let next_state' = advance state deltas num_cycles in
          Stdio.printf "[%d] advanced to %d\n" t
            (t + di + (num_cycles * (dj - di)));
          let t_explode, state' =
            advance_to_next_explosion g ~explode_at next_state'
          in
          let next_t = t + di + (num_cycles * (dj - di)) + t_explode in
          Some (next_t, (next_t, state'))))

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
