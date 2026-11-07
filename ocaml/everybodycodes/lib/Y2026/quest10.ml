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

type ghost = { direction : int * int; num_collisions : int }

let parse_lines l =
  let g = to_grid l in
  ( g,
    grid_foldi
      ~init:(Map.empty (module IntPair_Comparator))
      ~f:(fun coords acc ch ->
        match ch with
        | '>' ->
            Map.add_multi acc ~key:coords
              ~data:{ direction = (1, 0); num_collisions = 0 }
        | '<' ->
            Map.add_multi acc ~key:coords
              ~data:{ direction = (-1, 0); num_collisions = 0 }
        | '^' ->
            Map.add_multi acc ~key:coords
              ~data:{ direction = (0, -1); num_collisions = 0 }
        | 'v' ->
            Map.add_multi acc ~key:coords
              ~data:{ direction = (0, 1); num_collisions = 0 }
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
    Some { direction = (-dx, -dy); num_collisions = ghost.num_collisions + 1 }

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
                    direction = rotate_right (dx, dy);
                    num_collisions = ghost.num_collisions + 1;
                  }
          else Map.add_multi acc_m ~key:(nx, ny) ~data:ghost)
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

let part3 _ = 1
