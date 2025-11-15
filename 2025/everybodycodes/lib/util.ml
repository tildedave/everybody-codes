(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open Base

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let mod_positive n a =
  let m = n % a in
  if m >= 0 then m else m + a

let mod_exp a n m =
  (* a^n mod m *)
  let _a, p, _n = (ref a, ref 1, ref n) in
  while !_n > 0 do
    if equal_int (!_n % 2) 1 then p := !p * !_a % m;
    _a := !_a * !_a % m;
    _n := !_n / 2
  done;
  !p

let%test_unit "mod_exp" = [%test_eq: int] 12 (mod_exp 5 6 13)
let%test_unit "mod_exp" = [%test_eq: int] 8 (mod_exp 5 7 13)
let%test_unit "mod_exp" = [%test_eq: int] 1 (mod_exp 5 8 13)

let concat_ints l =
  Int.of_string @@ String.concat @@ List.map ~f:(Printf.sprintf "%d") l

let sum_list = List.fold ~f:( + ) ~init:0

(* https://stackoverflow.com/a/244104 *)
let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let set_add_all s = List.fold ~init:s ~f:Set.add

let string_to_int_list ?(on = ',') s =
  List.map ~f:Int.of_string @@ String.split ~on s
