open Base
open Util

let num_blocks num s =
  List.fold_right
    ~f:(fun n acc -> acc + (num / n))
    ~init:0 (string_to_int_list s)

let part1 = num_blocks 90
let%test_unit "part1 (given)" = [%test_eq: int] 193 (part1 "1,2,3,5,9")

(* simplest way for part2 seems to be going backwards, O(n) passes *)
let rec find_schematic l factors =
  match List.findi l ~f:(fun _ n -> n <> 0) with
  | None -> factors
  | Some (n, _) ->
      find_schematic
        (List.mapi
           ~f:(fun m x ->
             match x with
             | 0 -> x
             | _ -> if (m + 1) % (n + 1) = 0 then x - 1 else x)
           l)
        ((n + 1) :: factors)

let%test_unit "find_schematic (given)" =
  [%test_eq: int list] [ 9; 5; 3; 2; 1 ]
    (find_schematic
       [
         1;
         2;
         2;
         2;
         2;
         3;
         1;
         2;
         3;
         3;
         1;
         3;
         1;
         2;
         3;
         2;
         1;
         4;
         1;
         3;
         2;
         2;
         1;
         3;
         2;
         2;
       ]
       [])

let part2 s =
  List.fold_right ~init:1 ~f:( * ) (find_schematic (string_to_int_list s) [])

let%test_unit "part2 (given)" =
  [%test_eq: int] 270
    (part2 "1,2,2,2,2,3,1,2,3,3,1,3,1,2,3,2,1,4,1,3,2,2,1,3,2,2")

let part3 _ = 1
