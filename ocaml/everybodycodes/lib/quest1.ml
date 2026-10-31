open List
open Util
open Base

let parse_instruction s =
  (s.[0], Int.of_string (String.sub s ~pos:1 ~len:(String.length s - 1)))

let part1 lines =
  let names = String.split_on_chars (nth lines 0) ~on:[ ',' ] in
  let list_len = length names in
  let instructions = String.split_on_chars (nth lines 2) ~on:[ ',' ] in
  nth names
    (fold_left
       (fun acc curr ->
         match parse_instruction curr with
         | 'R', delta -> min (acc + delta) (list_len - 1)
         | 'L', delta -> max (acc - delta) 0
         | _ -> failwith "invalid instruction")
       0 instructions)

let%test_unit "part1" =
  [%test_eq: string] "Fyrryn"
    (part1
       (String.split_on_chars ~on:[ '\n' ]
          "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L1\n"))

let part2 lines =
  let names = String.split_on_chars (nth lines 0) ~on:[ ',' ] in
  let list_len = length names in
  let instructions = String.split_on_chars (nth lines 2) ~on:[ ',' ] in
  nth names
    (fold_left
       (fun acc curr ->
         match parse_instruction curr with
         | 'R', delta -> mod_positive (acc + delta) list_len
         | 'L', delta -> mod_positive (acc - delta) list_len
         | _ -> failwith "invalid instruction")
       0 instructions)

let%test_unit "part2" =
  [%test_eq: string] "Elarzris"
    (part2
       (String.split_on_chars ~on:[ '\n' ]
          "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L1\n"))

let part3 lines =
  let names = Array.of_list (String.split_on_chars ~on:[ ',' ] (nth lines 0)) in
  let names_len = Array.length names in
  let instructions = String.split_on_chars ~on:[ ',' ] (nth lines 2) in
  List.iter instructions ~f:(fun curr ->
      let idx =
        match parse_instruction curr with
        | 'R', delta -> mod_positive delta names_len
        | 'L', delta -> mod_positive (-delta) names_len
        | _ -> failwith "invalid instruction"
      in
      let old_head = Array.get names 0 in
      Array.set names 0 (Array.get names idx);
      Array.set names idx old_head);
  Array.get names 0

let%test_unit "part3" =
  [%test_eq: string] "Drakzyph"
    (part3
       (String.split_on_chars ~on:[ '\n' ]
          "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L3\n"))
