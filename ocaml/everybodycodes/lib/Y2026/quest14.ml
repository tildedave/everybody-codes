open Base
open Util

let example =
  [ "AAAEEEFFFF"; "AADDDDDGFF"; "BACDDIGGFF"; "BABBIIIGGH"; "BBBBIIIIHH" ]

(* I think we can just double count every connection / 2 then find grid bounds *)

let count_connections g =
  List.fold ~init:0
    ~f:(fun acc coord ->
      let coord_ch = grid_at g coord in
      List.fold ~init:acc
        ~f:(fun acc neighbor ->
          if not (equal_char (grid_at g neighbor) coord_ch) then acc + 1
          else acc)
        (grid_cardinal_neighbors g coord))
    (grid_all_coords g)

let part1 l =
  let g = to_grid l in
  let xmax, ymax = g.bounds in
  (count_connections g / 2) + (2 * xmax) + (2 * ymax)

let%test_unit "part1" = [%test_eq: int] 67 (part1 example)
let part2 _ = 1
let part3 _ = 1
