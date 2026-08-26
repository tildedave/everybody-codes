open Base

let parse_sequence s = List.map (String.split ~on:',' s) ~f:Int.of_string

let%test_unit "parse_sequence" =
  [%test_eq: int list] (parse_sequence "1,2,3,4,5") [ 1; 2; 3; 4; 5 ]

let run_sequence l =
  l
  |> List.fold
       ~init:(0, Set.empty (module Int))
       ~f:(fun (n, seen) inc ->
         let back_next, forward_next = (n - inc, n + inc) in
         if back_next > 0 && not (Set.mem seen back_next) then
           (back_next, Set.add seen back_next)
         else (forward_next, Set.add seen forward_next))
  |> fst

let%test_unit "run_sequence (given)" =
  [%test_eq: int] (run_sequence @@ parse_sequence "1,2,3,4,5,6,7,8,9") 21

let%test_unit "run_sequence (given 2)" =
  [%test_eq: int]
    (run_sequence
    @@ parse_sequence
         "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30"
    )
    45

let quest4part1 lines =
  lines
  |> List.map ~f:(fun s -> run_sequence @@ parse_sequence s)
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest4part1 (given)" =
  [%test_eq: int]
    (quest4part1
       [
         "1,2,3,4,5,6,7,8,9";
         "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30";
       ])
    66

let%test_unit "quest4part1 (given 2)" =
  [%test_eq: int]
    (quest4part1
       [
         "1,1,1,1,1";
         "5,1,2,3,4,5,1,2,3,4";
         "2,1,1,2,1,1,2,1,1,2,1,1";
         "5,1,2,1,2,7,1,2,1,2,7,1,2,1,2";
       ])
    34

let run_sequence_part2 l =
  l
  |> List.fold
       ~init:(0, Set.empty (module Int))
       ~f:(fun (n, seen) inc ->
         let back_next = n - inc in
         if back_next > 0 && not (Set.mem seen back_next) then
           (back_next, Set.add seen back_next)
         else
           let forward_next =
             Sequence.unfold ~init:(n + inc) ~f:(fun n -> Some (n, n + 1))
             |> Sequence.drop_while ~f:(Set.mem seen)
             |> Sequence.hd_exn
           in
           (forward_next, Set.add seen forward_next))
  |> fst

let%test_unit "run_sequence_part2 (given)" =
  [%test_eq: int] (run_sequence_part2 @@ parse_sequence "1,1,1,1,1") 5

let%test_unit "run_sequence_part2 (given 2)" =
  [%test_eq: int]
    (run_sequence_part2 @@ parse_sequence "5,1,2,3,4,5,1,2,3,4")
    13

let quest4part2 lines =
  lines
  |> List.map ~f:(fun s -> run_sequence_part2 @@ parse_sequence s)
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest4part2 (given)" =
  [%test_eq: int]
    (quest4part2
       [
         "1,1,1,1,1";
         "5,1,2,3,4,5,1,2,3,4";
         "2,1,1,2,1,1,2,1,1,2,1,1";
         "5,1,2,1,2,7,1,2,1,2,7,1,2,1,2";
       ])
    43

(* part 3 asks us to care about "crossings".
    easy to detect a crossing: if going from x to y, there's a crossing if an
    existing line begins or ends in the middle.

    only difficulty seems to be the forward jump increment logic, as written it is
    infinite.  I suppose if we are ever checking above the max point and can't
    make a jump without crossing something, we can give up.

    so we keep track of backwards and forwards jumps differently, as pairs.
    let's do it
*)

let avoids_crossings (x, y) arcs =
  if not (x < y) then assert false (* should do this better *)
  else
    arcs
    |> List.filter ~f:(fun (h, t) ->
           let h', t' = (min h t, max h t) in
           if h' < x && x < y && y < t' then false (* does not cross *)
           else if x < h' && h' < t' && t' < y then false
             (* also does not cross *)
           else (h' < x && x < t') || (h' < y && y < t'))
    |> List.is_empty

let run_sequence_part3 l =
  l
  |> List.fold
       ~init:(0, true, Set.empty (module Int), [], [])
       ~f:(fun (n, is_bottom, seen, top_arcs, bottom_arcs) inc ->
         let relevant_arcs = if is_bottom then bottom_arcs else top_arcs in
         let back_next = n - inc in
         if
           back_next > 0
           && (not (Set.mem seen back_next))
           && avoids_crossings (back_next, n) relevant_arcs
         then
           ( back_next,
             not is_bottom,
             Set.add seen back_next,
             (if is_bottom then top_arcs else (back_next, n) :: top_arcs),
             if is_bottom then (back_next, n) :: bottom_arcs else bottom_arcs )
         else
           let max_seen = Set.fold seen ~init:(n + inc) ~f:max in
           Sequence.unfold ~init:(n + inc) ~f:(fun n ->
               (* + 100 is a kludge, I want to end the process at some point *)
               if n > max_seen + 100 then None else Some (n, n + 1))
           |> Sequence.drop_while ~f:(fun n' ->
                  Set.mem seen n'
                  || not (avoids_crossings (n, n') relevant_arcs))
           |> Sequence.hd
           |> Option.value_map
                ~default:(n, is_bottom, seen, top_arcs, bottom_arcs)
                ~f:(fun forward_next ->
                  ( forward_next,
                    not is_bottom,
                    Set.add seen forward_next,
                    (if is_bottom then top_arcs
                     else (n, forward_next) :: top_arcs),
                    if is_bottom then (n, forward_next) :: bottom_arcs
                    else bottom_arcs )))
  |> fun (n, _, _, _, _) -> n

let%test_unit "run_sequence_part3 (given)" =
  [%test_eq: int] (run_sequence_part3 @@ parse_sequence "1,1,1,1,1") 5

let%test_unit "run_sequence_part3 (given, 2)" =
  [%test_eq: int]
    (run_sequence_part3 @@ parse_sequence "5,1,2,3,4,5,1,2,3,4")
    20

let quest4part3 lines =
  lines
  |> List.map ~f:(fun s -> run_sequence_part3 @@ parse_sequence s)
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest4part3 (given)" =
  [%test_eq: int]
    (quest4part3
       [
         "1,1,1,1,1";
         "5,1,2,3,4,5,1,2,3,4";
         "2,1,1,2,1,1,2,1,1,2,1,1";
         "5,1,2,1,2,7,1,2,1,2,7,1,2,1,2";
       ])
    27

let%test_unit "quest4part3 (given, part 2)" =
  [%test_eq: int]
    (quest4part3
       [
         "5,3,1,1";
         "5,3,1,1,5,1,1,3,4,8,1,1";
         "5,3,1,1,5,1,1,3,4,8,2,1";
         "10,9,9,8,8,7,7,6,6,5,5,4,4,3,3,2,2,1";
       ])
    35
