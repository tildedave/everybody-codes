open Base

(* answer is the isqrt + 1 *)

let answer seq n =
  (seq |> Sequence.drop_while ~f:(fun next -> n > next) |> Sequence.hd_exn) - n

let part1_seq = Sequence.unfold ~init:1 ~f:(fun n -> Some (n * n, n + 1))

let%test_unit "answer_part1 list" =
  [%test_eq: int list] [ 1; 5; 4; 0; 1; 0 ]
    (List.map ~f:(answer part1_seq) [ 3; 11; 12; 16; 24; 25 ])

let part2_seq = Sequence.unfold ~init:3 ~f:(fun n -> Some ((4 * n) - 4, n + 1))

let%test_unit "part2 sequence" =
  [%test_eq: int list] [ 8; 12; 16 ]
    (Sequence.to_list (Sequence.take part2_seq 3))

let%test_unit "answer_part2 list" =
  [%test_eq: int list] [ 5; 1; 0; 0; 0; 3 ]
    (List.map ~f:(answer part2_seq) [ 3; 11; 12; 16; 24; 25 ])

let part3_seq = Sequence.unfold ~init:3 ~f:(fun n -> Some ((n * n) - 1, n + 2))

let%test_unit "answer_part3 list" =
  [%test_eq: int list] [ 5; 13; 12; 8; 0; 23 ]
    (List.map ~f:(answer part3_seq) [ 3; 11; 12; 16; 24; 25 ])

let solve seq l =
  l |> String.split ~on:',' |> List.map ~f:Int.of_string
  |> List.map ~f:(answer seq)
  |> List.fold ~f:( + ) ~init:0

let part1 = solve part1_seq
let part2 = solve part2_seq
let part3 = solve part3_seq
