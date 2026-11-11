open Base
open Util

let parse_input lines =
  ( Stdlib.Scanf.sscanf (List.hd_exn lines) "portals: %d" Fn.id,
    List.fold
      ~init:(Map.empty (module Int))
      ~f:(fun acc s ->
        Stdlib.Scanf.sscanf s "%d %d" (fun d1 d2 ->
            Map.add_multi (Map.add_multi acc ~key:d2 ~data:d1) ~key:d1 ~data:d2))
      (List.drop lines 2) )

let example = [ "portals: 10"; ""; "5 10"; "2 7" ]

let%test_unit "parse_input" =
  [%test_result: int * (int * int list) list]
    ~expect:(10, [ (2, [ 7 ]); (5, [ 10 ]); (7, [ 2 ]); (10, [ 5 ]) ])
    ((fun (n, m) -> (n, Map.to_alist m)) (parse_input example))

(* presumably eventually we won't be able to enumerate the numbers *)
(* this is a straightforward dynamic programming problem.  we will use an array
   instead of a monadic map because my head hurts *)

let walk ~num_portals dx x = mod_positive (x - 1 + dx) num_portals + 1
let all_portals ~num_portals = Sequence.range ~stop:`inclusive 1 num_portals

let%test_unit "walk (forward)" =
  [%test_result: int list]
    ~expect:[ 2; 3; 4; 5; 6; 7; 8; 9; 10; 1 ]
    (Sequence.to_list
       (Sequence.map ~f:(walk ~num_portals:10 1) (all_portals ~num_portals:10)))

let%test_unit "walk (backwards)" =
  [%test_result: int list]
    ~expect:[ 10; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    (Sequence.to_list
       (Sequence.map
          ~f:(walk ~num_portals:10 (-1))
          (all_portals ~num_portals:10)))

(* let neighbors n ~config =
  let num_portals, jumps = config in
  let nextdoor_neighbors = [ walk num_portals 1 n; walk num_portals (-1) n ] in
  match Map.find jumps n with
  | None -> nextdoor_neighbors
  | Some x -> x :: nextdoor_neighbors *)

(* of course this is an unbounded search.  we have to be smarter.
   let us floyd-warshall *)
(*
if dist[i][j] > dist[i][k] + dist[k][j]
                dist[i][j] = dist[i][k] + dist[k][j]
*)

let all_distances ~num_portals m =
  let rng = Sequence.range 1 num_portals ~stop:`inclusive in
  let indices =
    let open Sequence.Let_syntax in
    let%bind k = rng in
    let%bind i = rng in
    let%map j = rng in
    (k, i, j)
  in
  Sequence.fold ~init:m indices ~f:(fun m (k, i, j) ->
      match (Map.find m (i, k), Map.find m (k, j)) with
      | Some d1, Some d2 ->
          Map.update m (i, j) ~f:(function
            | None -> d1 + d2
            | Some d' -> if d1 + d2 < d' then d1 + d2 else d')
      | _ -> m)

(* hypothesis, we have modified (x, y), we want to see if any
       (a, y) -- (a, x) + (x, y) needs updating
      looks like I may also have to solve
      (x, a) -- (x, y) + (y, a)
       fold over all rng, check if <, if <, recurse.
       unclear if this will work but I will try :-)
 *)
let repair_distances distances repair_set ~num_portals =
  let rng = Sequence.range 1 num_portals ~stop:`inclusive in
  let rec dfs distances (x, y) =
    Sequence.fold ~init:distances
      ~f:(fun distances a ->
        if equal a x || equal a y then distances
        else
          let distances =
            let d1 = Map.find_exn distances (a, x) in
            let d2 = Map.find_exn distances (x, y) in
            let d = Map.find_exn distances (a, y) in
            if d > d1 + d2 then
              dfs
                (distances
                |> Map.set ~key:(a, y) ~data:(d1 + d2)
                |> Map.set ~key:(y, a) ~data:(d1 + d2))
                (a, y)
            else distances
          in
          let d1 = Map.find_exn distances (x, y) in
          let d2 = Map.find_exn distances (y, a) in
          let d = Map.find_exn distances (x, a) in
          if d > d1 + d2 then
            dfs
              (distances
              |> Map.set ~key:(x, a) ~data:(d1 + d2)
              |> Map.set ~key:(a, x) ~data:(d1 + d2))
              (x, a)
          else distances)
      rng
  in
  Set.fold ~init:distances ~f:dfs repair_set

let p1_distances ~config =
  let num_portals, pair_list = config in
  ( Map.empty (module IntPair_Comparator) |> fun m ->
    List.fold ~init:m
      ~f:(fun m i ->
        m
        |> Map.set ~key:(i, i) ~data:0
        |> Map.set ~key:(i, walk ~num_portals 1 i) ~data:1
        |> Map.set ~key:(walk ~num_portals 1 i, i) ~data:1)
      (List.range 1 num_portals ~stop:`inclusive) )
  |> fun m ->
  List.fold ~init:m
    ~f:(fun m (source, dest_list) ->
      List.fold ~init:m
        ~f:(fun m dest -> Map.set m ~key:(source, dest) ~data:1)
        dest_list)
    (Map.to_alist pair_list)

let distance_from x m ~num_portals =
  Sequence.map (all_portals ~num_portals) ~f:(fun dest ->
      Map.find_exn m (x, dest))

let total_distance x ~m ~num_portals =
  Sequence.sum (module Int) (distance_from x m ~num_portals) ~f:Fn.id

let no_portals config = (fst config, Map.empty (module Int))

let%test_unit "distance_from (no portals)" =
  [%test_result: int list]
    ~expect:[ 0; 1; 2; 3; 4; 5; 4; 3; 2; 1 ]
    (let config = no_portals (parse_input example) in
     Sequence.to_list
       (distance_from 1
          (all_distances ~num_portals:(fst config) (p1_distances ~config))
          ~num_portals:(fst config)))

let%test_unit "distance_from (portals)" =
  [%test_result: int list]
    ~expect:[ 0; 1; 2; 3; 2; 3; 2; 3; 2; 1 ]
    (let config = parse_input example in
     Sequence.to_list
       (distance_from 1
          (all_distances ~num_portals:(fst config) (p1_distances ~config))
          ~num_portals:(fst config)))

let part1 l =
  let config = parse_input l in
  let num_portals, _ = config in
  let d_portals = all_distances ~num_portals (p1_distances ~config) in
  let d_no_portals =
    all_distances ~num_portals (p1_distances ~config:(no_portals config))
  in
  Sequence.fold ~init:0
    ~f:(fun acc n ->
      acc
      + (total_distance n ~num_portals ~m:d_no_portals
        - total_distance n ~num_portals ~m:d_portals))
    (all_portals ~num_portals)

let%test_unit "part1" = [%test_result: int] ~expect:60 (part1 example)

let p2_distances ~config =
  let num_portals, pair_list = config in
  ( Map.empty (module IntPair_Comparator) |> fun m ->
    List.fold ~init:m
      ~f:(fun m i ->
        m |> Map.set ~key:(i, i) ~data:0 |> fun m ->
        List.fold ~init:m
          ~f:(fun m dx ->
            m
            |> Map.set ~key:(i, walk ~num_portals dx i) ~data:1
            |> Map.set ~key:(walk ~num_portals dx i, i) ~data:1)
          (List.range 1 4))
      (List.range 1 num_portals ~stop:`inclusive) )
  |> fun m ->
  List.fold ~init:m
    ~f:(fun m (source, dest_list) ->
      List.fold ~init:m
        ~f:(fun m dest -> Map.set m ~key:(source, dest) ~data:1)
        dest_list)
    (Map.to_alist pair_list)

let%test_unit "distance_from (p2_distances)" =
  [%test_result: int list]
    ~expect:[ 0; 1; 1; 1; 2; 2; 2; 1; 1; 1 ]
    (let config = parse_input example in
     Sequence.to_list
       (distance_from 1
          (all_distances ~num_portals:(fst config) (p2_distances ~config))
          ~num_portals:(fst config)))

(* OK part2 is basically part1 with some optimizing *)
let part2 l =
  let config = parse_input l in
  let num_portals, _ = config in
  let no_portal_distances = p2_distances ~config:(no_portals config) in
  let rng = Sequence.range 1 num_portals ~stop:`inclusive in
  let initial_distances = all_distances no_portal_distances ~num_portals in
  (* observation: total weight is just the sum of the entire matrix *)
  let no_portal_weight = Map.sum (module Int) ~f:Fn.id initial_distances in
  let portal_distances = p2_distances ~config in
  let optimize_indices =
    Sequence.filter_opt
    @@
    let open Sequence.Let_syntax in
    let%bind x = rng in
    let%map y = rng in
    if equal x y || x > y then None
    else
      match Map.find portal_distances (x, y) with
      | None -> Some (x, y)
      | Some 1 -> None
      | Some _ -> Some (x, y)
  in
  let portal_all_distances = all_distances ~num_portals portal_distances in
  optimize_indices
  |> Sequence.map ~f:(fun (x, y) ->
      repair_distances
        (portal_all_distances
        |> Map.set ~key:(x, y) ~data:1
        |> Map.set ~key:(y, x) ~data:1)
        (Set.empty (module IntPair_Comparator)
        |> (Fn.flip Set.add) (x, y)
        |> (Fn.flip Set.add) (y, x))
        ~num_portals
      |> (fun m -> Sequence.map ~f:(total_distance ~m ~num_portals) rng)
      |> Sequence.sum (module Int) ~f:Fn.id)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> fun v -> no_portal_weight - v

let%test_unit "part2" = [%test_result: int] ~expect:6 (part2 example)

(* well part2 didn't finish instantly.  part3 is 2x as big, but the number of
   indices to check really just remain linear in the size of the ring. *)
(* my part2 solution is not completing on part3, so we may have to optimize more *)
(* I believe we can be smarter with floyd warshall. *)
(* OK we are smarter with floyd warshall.  excellent.  88 / 8 = only 11
   candidates, which is I think less work than my existing code! *)
(* j/k the candidate logic is more complicated *)

let spaced_connections start ~num_portals =
  assert (equal (num_portals % 8) 0);
  let dn = num_portals / 8 in
  (* all connections must be dn from each other *)
  let spots = List.init 8 ~f:(fun n -> start + (n * dn)) in
  (* must emit all combinations of 4 pairs of these elements *)
  let rec helper = function
    | [] -> Sequence.singleton Sequence.empty
    | x :: xs ->
        Sequence.concat_map (Sequence.of_list xs) ~f:(fun y ->
            let remaining = List.filter xs ~f:(fun y' -> not (equal y y')) in
            Sequence.map (helper remaining) ~f:(fun m ->
                Sequence.shift_right m (x, y)))
  in
  helper spots

(* let%test_unit "spaced_connections" =
  [%test_result: (int * int) list list] ~expect:[]
    (spaced_connections ~num_portals:32 2)

let%test_unit "spaced_connections (size 64)" =
  [%test_result: (int * int) list list]
    ~expect:
      [
        [
          (1, 17);
          (9, 33);
          (10, 26);
          (14, 30);
          (18, 2);
          (22, 6);
          (26, 10);
          (30, 14);
        ];
      ]
    (spaced_connections ~num_portals:64 1) *)

let part3 l =
  let config = parse_input l in
  let num_portals, _ = config in
  let no_portal_distances = p2_distances ~config:(no_portals config) in
  let rng = Sequence.range 1 num_portals ~stop:`inclusive in
  let initial_distances = all_distances no_portal_distances ~num_portals in
  (* observation: total weight is just the sum of the entire matrix *)
  let no_portal_weight = Map.sum (module Int) ~f:Fn.id initial_distances in
  let portal_distances = p2_distances ~config in
  let candidates =
    (Sequence.concat_map ~f:(spaced_connections ~num_portals))
      (Sequence.take rng (num_portals / 8))
  in
  let portal_all_distances = all_distances ~num_portals portal_distances in
  candidates
  |> Sequence.map ~f:(fun connections ->
      let connection_pairs =
        Sequence.fold
          ~init:(Set.empty (module IntPair_Comparator))
          ~f:(fun acc (x, y) ->
            acc |> (Fn.flip Set.add) (x, y) |> (Fn.flip Set.add) (y, x))
          connections
      in
      repair_distances
        (Set.fold ~init:portal_all_distances
           ~f:(fun m (x, y) ->
             m |> Map.set ~key:(x, y) ~data:1 |> Map.set ~key:(y, x) ~data:1)
           connection_pairs)
        connection_pairs ~num_portals
      |> (fun m -> Sequence.map ~f:(total_distance ~m ~num_portals) rng)
      |> Sequence.sum (module Int) ~f:Fn.id)
    (* |> fun v ->
      Stdio.printf "connections %s --> %d\n"
        (String.concat ~sep:","
        @@ List.map
             ~f:(fun (d1, d2) -> Printf.sprintf "(%d, %d)" d1 d2)
             (Sequence.to_list connections))
        v;
      Out_channel.flush Stdio.stdout;
      v) *)
  |> Sequence.min_elt ~compare:Int.compare
  |> Option.value_exn
  |> fun v -> no_portal_weight - v

(* let p3_example1 = [ "portals: 32"; ""; "1 10"; "20 30"; "15 29"; "13 10" ]

let%test_unit "part3 (first example)" =
  [%test_result: int] ~expect:854 (part3 p3_example1) *)

(* too slow *)
(* let p3_example2 = [ "portals: 64"; ""; "1 40"; "3 35"; "5 42"; "7 50" ] *)
(* let%test_unit "part3 (second example)" =
  [%test_result: int] ~expect:9634 (part3 p3_example2) *)
