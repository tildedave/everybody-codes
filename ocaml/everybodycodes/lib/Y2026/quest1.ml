open Base

(* answer is the isqrt + 1 *)

let isqrt n =
  let rec isqrt_helper lo hi =
    if equal lo (hi - 1) then lo
    else
      let mid = (lo + hi) / 2 in
      if mid * mid <= n then isqrt_helper mid hi else isqrt_helper lo mid
  in
  isqrt_helper 0 (n + 1)

let answer_part1 n =
  let sq = isqrt n in
  if equal n (sq ** 2) then 0 else ((sq + 1) ** 2) - n

let%test_unit "answer_part1 list" =
  [%test_eq: int list] [ 1; 5; 4; 0; 1; 0 ]
    (List.map ~f:answer_part1 [ 3; 11; 12; 16; 24; 25 ])

let part2_seq = Sequence.unfold ~init:3 ~f:(fun n -> Some ((4 * n) - 4, n + 1))

let%test_unit "part2 sequence" =
  [%test_eq: int list] [ 8; 12; 16 ]
    (Sequence.to_list (Sequence.take part2_seq 3))

let answer_part2 n =
  (* an lxl block uses 4l - 4 pieces *)
  (part2_seq |> Sequence.drop_while ~f:(fun next -> n > next) |> Sequence.hd_exn)
  - n

let%test_unit "answer_part2 list" =
  [%test_eq: int list] [ 5; 1; 0; 0; 0; 3 ]
    (List.map ~f:answer_part2 [ 3; 11; 12; 16; 24; 25 ])

let part1 l =
  l |> String.split ~on:',' |> List.map ~f:Int.of_string
  |> List.map ~f:answer_part1 |> List.fold ~f:( + ) ~init:0

let part2 l =
  l |> String.split ~on:',' |> List.map ~f:Int.of_string
  |> List.map ~f:answer_part2 |> List.fold ~f:( + ) ~init:0
