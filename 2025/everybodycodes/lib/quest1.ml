open List;;
open Util;;

let quest1 lines =
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

let rec mod_positive n a = let m = n mod a in if m >= 0 then m else m + a;;

let quest2 lines =
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

let quest3 lines =
  let names = Array.of_list (String.split_on_char ',' (nth lines 0)) in
  let names_len = Array.length names in
  let instructions = (String.split_on_char ',' (nth lines 2)) in
    begin
    List.iter (fun curr ->
      (* let _ = Printf.printf "[%s]" curr in
      let _ = List.iter (Printf.printf "%s ") (Array.to_list names) in
      let _ = Printf.printf "\n" in *)
      let delta = int_of_string(String.sub curr 1 ((String.length curr) - 1)) in
      let idx = match (curr.[0]) with
      | 'R' -> mod_positive delta names_len
      | 'L' -> mod_positive (-delta) names_len
      | _ -> failwith "invalid instruction" in
      let old_head = Array.get names 0 in
      let _ = Printf.printf "idx %d len %d\n" idx names_len in
      begin
      Array.set names 0 (Array.get names idx);
      Array.set names idx old_head
      end) instructions;
      Array.get names 0
    end;;


print_endline "EXAMPLES:";;
print_endline (quest1 (String.split_on_char '\n' "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L1\n"));;
print_endline (quest2 (String.split_on_char '\n' "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L1\n"));;
print_endline (quest3 (String.split_on_char '\n' "Vyrdax,Drakzyph,Fyrryn,Elarzris\n\nR3,L2,R3,L3\n"));;

print_endline "ANSWERS:";;
print_endline (quest1 (read_lines "quest-notes/quest1-part1.txt"));;
print_endline (quest2 (read_lines "quest-notes/quest1-part2.txt"));;
print_endline (quest3 (read_lines "quest-notes/quest1-part3.txt"));;
