[@@@ocaml.warning "-31-32"]

open Base
open Core

let phase1 arr =
  let mutated = ref false in
  for i = 0 to Array.length arr - 2 do
    if arr.(i) > arr.(i + 1) then (
      Array.set arr i (arr.(i) - 1);
      Array.set arr (i + 1) (arr.(i + 1) + 1);
      mutated := true)
  done;
  !mutated

let phase2 arr =
  let mutated = ref false in
  for i = 0 to Array.length arr - 2 do
    if arr.(i) < arr.(i + 1) then (
      Array.set arr i (arr.(i) + 1);
      Array.set arr (i + 1) (arr.(i + 1) - 1);
      mutated := true)
  done;
  !mutated

let to_arr l = Array.of_list @@ List.map ~f:Int.of_string l

let flock_checksum arr =
  List.foldi ~f:(fun n acc m -> acc + ((n + 1) * m)) ~init:0 arr

let%test_unit "flock checksum" =
  [%test_eq: int] 109 (flock_checksum [ 4; 5; 5; 5; 6; 5 ])

let run_to_round dest_round l =
  let arr = to_arr l in
  let rec loop round_num round1 =
    let mutated_any = if round1 then phase1 arr else phase2 arr in
    if round_num = dest_round then Array.to_list arr
    else
      loop (round_num + 1) (if round1 && not mutated_any then false else round1)
  in
  loop 0 true

let is_balanced arr =
  with_return (fun r ->
      let seen = ref None in
      Array.iter
        ~f:(fun x ->
          match !seen with
          | None -> seen := Some x
          | Some y -> if x <> y then r.return false)
        arr;
      true)

let%test_unit "run_to_round" =
  [%test_eq: int] 109
    (flock_checksum (run_to_round 10 [ "9"; "1"; "1"; "4"; "9"; "6" ]))

let part1 l = flock_checksum (run_to_round 10 l)

let run_to_balanced l =
  let arr = to_arr l in
  let rec loop round_num round1 =
    (* Stdio.printf "[%d] %s\n" round_num *)
    (* (String.concat ~sep:"; " (List.map ~f:Int.to_string (Array.to_list arr))); *)
    let mutated_any = if round1 then phase1 arr else phase2 arr in
    if is_balanced arr then round_num
    else
      loop (round_num + 1)
        (if round1 && not mutated_any then
           (* Stdio.printf "[%d] changing rounds %s\n" round_num
              (String.concat ~sep:"; "
                 (List.map ~f:Int.to_string (Array.to_list arr))); *)
           false
         else round1)
  in
  loop 0 true

let%test_unit "run_to_balanced" =
  [%test_eq: int] 11 (run_to_balanced [ "9"; "1"; "1"; "4"; "9"; "6" ])

let%test_unit "run_to_balanced (2)" =
  [%test_eq: int] 1579
    (run_to_balanced
       [
         "805";
         "706";
         "179";
         "48";
         "158";
         "150";
         "232";
         "885";
         "598";
         "524";
         "423";
       ])

let part2 = run_to_balanced

(* part3 has a trick.  the numbers are increasing so we don't have to worry
   about the part1 portion of the logic *)

let part3 l =
  let open Bigint in
  let ints = List.map ~f:(Fn.compose of_int Int.of_string) l in
  let sum = List.fold ~f:( + ) ~init:zero ints in
  let mean = sum / of_int (List.length l) in
  to_int_exn
  @@ List.fold
       ~f:(fun acc n -> if n < mean then acc + (mean - n) else acc)
       ~init:zero ints

let%test_unit "part 3" =
  [%test_eq: int] 1579
    (861
    + part3
        [
          "325";
          "325";
          "325";
          "325";
          "326";
          "326";
          "326";
          "607";
          "607";
          "608";
          "608";
        ])
