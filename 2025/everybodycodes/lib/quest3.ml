open Base

let parse_string s =
  List.map ~f:Int.of_string (String.split_on_chars s ~on:[ ',' ])

let%test_unit "parse_string" =
  [%test_eq: int list]
    [ 10; 5; 1; 10; 3; 8; 5; 2; 2 ]
    (parse_string "10,5,1,10,3,8,5,2,2")

let descending_seq_p1 l =
  let q = List.sort ~compare:(fun a b -> compare b a) l in
  let x, tl = (List.hd_exn q, List.tl_exn q) in
  let last, n =
    List.fold tl ~init:(x, 0) ~f:(fun (y, n) x ->
        if y > x then (x, n + y) else (x, n))
  in
  last + n

let%test_unit "descending_seq_p1" =
  [%test_eq: int] 29 (descending_seq_p1 [ 10; 5; 1; 10; 3; 8; 5; 2; 2 ])

let part1 s = Printf.sprintf "%d" (descending_seq_p1 (parse_string s))

let%test_unit "part1 given" =
  [%test_eq: string] "29" (part1 "10,5,1,10,3,8,5,2,2")

let ascending_seq_p2 l =
  let q = List.sort ~compare l in
  let x, tl = (List.hd_exn q, List.tl_exn q) in
  let last, n, _ =
    List.fold tl ~init:(x, 0, 1) ~f:(fun (y, n, c) x ->
        if equal c 20 then (y, n, c)
        else if x > y then (x, n + y, c + 1)
        else (x, n, c))
  in
  last + n

let part2 s = Printf.sprintf "%d" (ascending_seq_p2 (parse_string s))

let%test_unit "part2 given" =
  [%test_eq: string] "781"
    (part2
       "4,51,13,64,57,51,82,57,16,88,89,48,32,49,49,2,84,65,49,43,9,13,2,3,75,72,63,48,61,14,40,77")

(* actually don't need this, I think it's just max word frequency on the numbers *)
(*
let descending_seq_p3 l =
  let q = List.sort ~compare:(fun a b -> compare b a) l in
  let x, tl = (List.hd_exn q, List.tl_exn q) in
  let x, set, remaining_l =
    List.fold tl ~init:(x, [], []) ~f:(fun (y, set, remaining_l) x ->
        if y > x then (x, y :: set, remaining_l) else (y, set, x :: remaining_l))
  in
  (x :: set, remaining_l)

let%test_unit "descending_seq_p3" =
  [%test_eq: int list * int list]
    ([ 1; 2; 3 ], [ 3; 2; 4 ])
    (descending_seq_p3
       (parse_string
          "4,51,13,64,57,51,82,57,16,88,89,48,32,49,49,2,84,65,49,43,9,13,2,3,75,72,63,48,61,14,40,77")) *)

let frequency_p3 l =
  let q = List.sort ~compare:(fun a b -> compare b a) l in
  let x, tl = (List.hd_exn q, List.tl_exn q) in
  let m, _, n =
    List.fold tl ~init:(0, x, 1) ~f:(fun (max_so_far, y, n) x ->
        if y > x then if n > max_so_far then (n, x, 1) else (max_so_far, x, 1)
        else (max_so_far, y, n + 1))
  in
  if n > m then n else m

let%test_unit "frequency_p3" =
  [%test_eq: int] 3
    (frequency_p3
       (parse_string
          "4,51,13,64,57,51,82,57,16,88,89,48,32,49,49,2,84,65,49,43,9,13,2,3,75,72,63,48,61,14,40,77"))

let part3 s = Printf.sprintf "%d" (frequency_p3 (parse_string s))
