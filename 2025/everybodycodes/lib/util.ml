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

(* Knuth Algo X, 4.5.2 *)
let euclid_extended u v =
  let rec loop (u1, u2, u3) (v1, v2, v3) =
    if v3 = 0 then (u1, u2, u3)
    else
      let q = u3 / v3 in
      let t1, t2, t3 = (u1 - (v1 * q), u2 - (v2 * q), u3 - (v3 * q)) in
      loop (v1, v2, v3) (t1, t2, t3)
  in
  loop (1, 0, u) (0, 1, v)

let%test_unit "euclid extended" =
  [%test_eq: int * int * int] (337, -571, 34) (euclid_extended 40902 24140)

(* Course in Computational Number Theory, 1.3.12 *)
let crt_inductive residues =
  let rec loop (x, m) residues =
    Stdio.printf "%d mod %d\n" x m;
    match residues with
    | [] -> x
    | (xi, mi) :: l ->
        let u, v, d = euclid_extended m mi in
        Stdio.printf "%d * %d + %d * %d = %d (actually %d)\n" u m v mi d
          ((u * m) + (v * mi));
        assert ((u * m) + (v * mi) = d);
        let x, m = ((u * m * xi) + (v * mi * x), m * mi) in
        assert (d = 1);
        loop (x % m, m) l
  in
  Stdio.printf "residues: %s\n"
    (String.concat ~sep:";"
    @@ List.map ~f:(fun (n, p) -> Printf.sprintf "(%d, %d)" n p) residues);
  match residues with
  | (x, m) :: rest_residues -> loop (x, m) rest_residues
  | _ -> failwith "invalid argument"

let%test_unit "crt_inductive" =
  [%test_eq: int] 39 (crt_inductive [ (0, 3); (3, 4); (4, 5) ])
