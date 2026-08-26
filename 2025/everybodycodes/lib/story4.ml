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
