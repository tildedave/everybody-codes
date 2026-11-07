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

let parse_lines l =
  let g = to_grid l in
  ( g,
    grid_foldi
      ~init:(Map.empty (module IntPair_Comparator))
      ~f:(fun coords acc ch ->
        match ch with
        | '>' -> Map.add_multi acc ~key:coords ~data:(1, 0)
        | '<' -> Map.add_multi acc ~key:coords ~data:(-1, 0)
        | '^' -> Map.add_multi acc ~key:coords ~data:(0, -1)
        | 'v' -> Map.add_multi acc ~key:coords ~data:(0, 1)
        | _ -> acc)
      g )

let rotate_right (dx, dy) =
  match (dx, dy) with
  | 0, -1 -> (1, 0)
  | 0, 1 -> (-1, 0)
  | 1, 0 -> (0, 1)
  | -1, 0 -> (0, -1)
  | _ -> failwith "invalid deltas"

let flip (dx, dy) = (-dx, -dy)

let _print_map g m =
  for y = 0 to snd g.bounds - 1 do
    for x = 0 to fst g.bounds - 1 do
      if equal_char (grid_at g (x, y)) '#' then Stdio.printf "#"
      else
        match Map.find m (x, y) with
        | None -> Stdio.printf "."
        | Some [ x ] ->
            Stdio.printf "%s"
              (match x with
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

let move g m =
  (* _print_map g m; *)
  Map.fold
    ~init:(0, Map.empty (module IntPair_Comparator))
    ~f:(fun ~key:(x, y) ~data:ghosts (num_collisions, acc_m) ->
      List.fold ~init:(num_collisions, acc_m)
        ~f:(fun (num_collisions, acc_m) (dx, dy) ->
          let nx, ny = (x + dx, y + dy) in
          if equal_char (grid_at g (nx, ny)) '#' then
            ( num_collisions + 1,
              Map.add_multi acc_m ~key:(x, y) ~data:(rotate_right (dx, dy)) )
          else (num_collisions, Map.add_multi acc_m ~key:(nx, ny) ~data:(dx, dy)))
        ghosts)
    m

let collide (num_collisions, m) =
  (* bleh, fix this so we're better *)
  ( Map.fold ~init:num_collisions
      ~f:(fun ~key:_ ~data:ghosts acc ->
        let l = List.length ghosts in
        if l > 1 then acc + l else acc)
      m,
    Map.map
      ~f:(fun ghosts ->
        match ghosts with
        | [] -> failwith "impossible"
        | [ x ] -> [ x ]
        | _ -> List.map ~f:flip ghosts)
      m )

let ghost_seq l =
  let g, m = parse_lines l in
  Sequence.unfold ~init:m ~f:(fun m -> Some (m |> move g |> collide))

let part1 l =
  Sequence.sum (module Int) (Sequence.take (ghost_seq l) 10) ~f:Fn.id

let%test_unit "ghost_seq" =
  [%test_eq: int list]
    [ 4; 3; 1; 1; 8; 3; 1; 3; 1; 4 ]
    (Sequence.to_list (Sequence.take (ghost_seq example) 10))

let%test_unit "part1" = [%test_eq: int] 29 (part1 example)
let part2 _ = 1
let part3 _ = 1
