open Base
(* open Util *)

let rev_tuple (a, b) = (b, a)

let construct_wheel l =
  let cw, ccw =
    List.foldi l ~init:([], []) ~f:(fun n (cw, ccw) s ->
        let range =
          match Int.of_string_opt s with
          | Some n -> (n, n)
          | None -> Stdlib.Scanf.sscanf s "%d-%d" (fun lo hi -> (lo, hi))
        in
        match n % 2 with
        | 0 -> (range :: cw, ccw)
        | 1 -> (cw, rev_tuple range :: ccw)
        | _ -> failwith "impossible")
  in
  Array.of_list @@ ((1, 1) :: List.rev cw) @ ccw

let%test_unit "construct_wheel (given, part 1)" =
  [%test_eq: (int * int) list]
    [ (1, 1); (72, 72); (47, 47); (67, 67); (61, 61); (58, 58) ]
    (List.of_array (construct_wheel [ "72"; "58"; "47"; "61"; "67" ]))

let wheel_length =
  Array.fold ~init:0 ~f:(fun acc (lo, hi) -> acc + (Int.abs (hi - lo) + 1))

let traverse wheel num_steps =
  let rec loop i steps_left =
    let lo, hi = wheel.(i) in
    let steps_out = Int.abs (hi - lo) + 1 in
    if steps_out <= steps_left then
      loop ((i + 1) % Array.length wheel) (steps_left - steps_out)
    else if lo < hi then lo + steps_left
    else lo - steps_left
  in
  loop 0 (num_steps % wheel_length wheel)

let part1 l = traverse (construct_wheel l) 2025

(* let%test_unit "part1 (given)" =
   [%test_eq: int] 67 (part1 [ "72"; "58"; "47"; "61"; "67" ]) *)

let part2 l = traverse (construct_wheel l) 20252025

let%test_unit "wheel_length" =
  [%test_eq: int] 24
    (wheel_length
       (construct_wheel [ "10-15"; "12-13"; "20-21"; "19-23"; "30-37" ]))

let%test_unit "traverse" =
  [%test_eq: int list]
    [
      1;
      10;
      11;
      12;
      13;
      14;
      15;
      20;
      21;
      30;
      31;
      32;
      33;
      34;
      35;
      36;
      37;
      23;
      22;
      21;
      20;
      19;
      13;
      12;
    ]
    (let wheel =
       construct_wheel [ "10-15"; "12-13"; "20-21"; "19-23"; "30-37" ]
     in
     List.init 24 ~f:(fun n -> traverse wheel n))

let%test_unit "part2 (given)" =
  [%test_eq: int] 30 (part2 [ "10-15"; "12-13"; "20-21"; "19-23"; "30-37" ])

let part2 l = traverse (construct_wheel l) 20252025
let part3 l = traverse (construct_wheel l) 202520252025
