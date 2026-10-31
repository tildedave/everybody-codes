open Base
open Util

let num_crosses nail_count s =
  string_to_int_list s
  |> List.fold
       ~f:(fun (last, num_crosses) curr ->
         if equal (Int.abs (curr - last)) (nail_count / 2) then
           (curr, num_crosses + 1)
         else (curr, num_crosses))
       ~init:(0, 0)
  |> snd

let%test_unit "num_crosses" =
  [%test_eq: int] 4 (num_crosses 8 "1,5,2,6,8,4,1,7,3")

let part1 = num_crosses 32

(* unfortunately, gotta think for part 2 ;-) *)
(* these are roots of unity on the circle, e^(2pi*i / num_nails) *)
(* closed form will probably "collapse" *)
(* can do it all as vectors over complex numbers *)

(* seems like we can be smarter *)
(* (a,b) intersects (c,d) if *)
(*  (1,4) intersects (2,6) and (2,5) but not (4,8) and not (6,8) *)
(* interval logic? *)
(* (4,8) intersects (2,6) and (2,5) and (1,5) but not (6, 8) *)
(* (3,5) intersects (4,8) and (1,4) but not (2,6), (3,7), (2,5) *)

let intersects p1 p2 =
  match (p1, p2) with
  | (start1, finish1), (start2, finish2) ->
      if start2 < start1 then
        start2 < start1 && start1 < finish2 && finish2 < finish1
      else start1 < start2 && start2 < finish1 && finish1 < finish2

let pairs s =
  string_to_int_list s
  |> List.fold
       ~f:(fun (last, sofar) curr ->
         (curr, (if last < curr then (last, curr) else (curr, last)) :: sofar))
       ~init:(0, [])
  |> snd

let part2 s =
  pairs s
  |> List.fold
       ~f:(fun (knots, sofar) p1 ->
         ( knots
           + (sofar
             |> List.filter ~f:(fun p2 -> intersects p1 p2)
             |> List.length),
           p1 :: sofar ))
       ~init:(0, [])
  |> fst

let%test_unit "part2 given" =
  [%test_eq: int] 21 (part2 "1,5,2,6,8,4,1,7,3,5,7,8,2")

let all_pairs nail_count =
  List.cartesian_product (1 -- nail_count) (1 -- nail_count)
  |> List.filter ~f:(fun (a, b) -> a < b)

let equal_points (n1, n2) (m1, m2) = equal n1 m1 && equal n2 m2

let _part3 nail_count s =
  let lines =
    pairs s
    |> List.fold
         ~f:(fun (knots, sofar) p1 ->
           ( knots
             + (sofar
               |> List.filter ~f:(fun p2 -> intersects p1 p2)
               |> List.length),
             p1 :: sofar ))
         ~init:(0, [])
    |> snd
  in
  all_pairs nail_count
  |> List.map ~f:(fun p1 ->
         lines
         |> List.filter ~f:(fun p2 -> intersects p1 p2 || equal_points p1 p2)
         |> List.length)
  |> List.max_elt ~compare |> Option.value ~default:0

let%test_unit "part3 given" = [%test_eq: int] 7 (_part3 8 "1,5,2,6,8,4,1,7,3,6")
let part3 = _part3 256
