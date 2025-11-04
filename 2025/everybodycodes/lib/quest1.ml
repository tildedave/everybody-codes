open List;;
open Util;;
open Base;;

let part1 lines =
  let names = (String.split_on_char ',' (nth lines 0)) in
  let list_len = length names in
  let instructions = (String.split_on_char ',' (nth lines 2)) in
  nth
  names
  (fold_left
    (fun acc curr ->
      let delta = int_of_string(String.sub curr 1 1) in
      match (curr.[0]) with
      | 'R' -> min (acc + delta) (list_len - 1)
      | 'L' -> max (acc - delta) 0
      | _ -> failwith "invalid instruction"
      )
    0
  instructions);;

let part2 lines =
  let names = (String.split_on_char ',' (nth lines 0)) in
  let list_len = length names in
  let instructions = (String.split_on_char ',' (nth lines 2)) in
  nth
  names
  (fold_left
    (fun acc curr ->
      let delta = int_of_string(String.sub curr 1 ((String.length curr) - 1)) in
      match (curr.[0]) with
      | 'R' -> mod_positive (acc + delta) list_len
      | 'L' -> mod_positive (acc - delta) list_len
      | _ -> failwith "invalid instruction"
      )
    0
  instructions);;

let part3 lines =
  let names = Array.of_list (String.split_on_char ',' (nth lines 0)) in
  let names_len = Array.length names in
  let instructions = (String.split_on_char ',' (nth lines 2)) in
    begin
    List.iter (fun curr ->
      let delta = int_of_string(String.sub curr 1 ((String.length curr) - 1)) in
      let idx = match (curr.[0]) with
      | 'R' -> mod_positive delta names_len
      | 'L' -> mod_positive (-delta) names_len
      | _ -> failwith "invalid instruction" in
      let old_head = Array.get names 0 in
      begin
      Array.set names 0 (Array.get names idx);
      Array.set names idx old_head
      end) instructions;
      Array.get names 0
    end;;


(* let%expect_test _ =
    print_endline (part1 (String.split_on_char '\n' "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L1\n"));
    [%expect{|
      Hello, world!
    |}];; *)

(* print_endline "EXAMPLES:";;
print_endline ;;
print_endline (part2 (String.split_on_char '\n' "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L1\n"));;
print_endline (part3 (String.split_on_char '\n' "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L3\n"));; *)
