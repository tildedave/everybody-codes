open Base
open Util

let num_blocks wall_length schematic =
  List.fold_right ~f:(fun n acc -> acc + (wall_length / n)) ~init:0 schematic

let part1 s = num_blocks 90 @@ string_to_int_list s
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

(* approach will be binary-search-ish from the pattern, * 10 *)
(* length of wall --> num_blocks *)

let find_blocks goal s =
  let schematic = find_schematic (string_to_int_list s) [] in
  let rec loop lower upper =
    (* Stdio.printf "(%d, %d) --> next = %d, ordering %d\n%!" lower upper
       (lower + ((upper - lower) / 2))
       (compare (num_blocks (lower + ((upper - lower) / 2)) schematic) goal); *)
    if upper - lower = 1 then lower
    else
      let next = lower + ((upper - lower) / 2) in
      match Ordering.of_int (compare (num_blocks next schematic) goal) with
      | Less -> loop next upper
      | Equal -> next
      | Greater -> loop lower next
  in
  loop 0 202520252025000

let part3 = find_blocks 202520252025000

let%test_unit "part3 (example)" =
  [%test_eq: int] 100000 (num_blocks 46633 [ 1; 2; 3; 5; 9 ])

let%test_unit "part3 (given)" =
  [%test_eq: int] 94439495762954
    (part3 "1,2,2,2,2,3,1,2,3,3,1,3,1,2,3,2,1,4,1,3,2,2,1,3,2,2")
