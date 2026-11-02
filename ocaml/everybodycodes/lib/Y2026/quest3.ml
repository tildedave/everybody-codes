open Base

let string_to_nums s =
  s
  |> String.fold ~init:[] ~f:(fun acc ch ->
      (Int.of_string @@ Char.to_string ch) :: acc)
  |> List.rev

let%test_unit "string_to_nums" =
  [%test_eq: int list] [ 3; 4; 5; 2 ] (string_to_nums "3452")

(* part3 is simple too I think - given a sequence and a number we convert to
   ISO-2, then compare equality - if the same good, otherwise discard *)
(* for each number + power mod the number by the digit and reduce mod power *)

(* let's think about how 213215 would be converted to base 10 -- starting from
   the right.

   ((10, 0), 5) --> 213215 -> mod 10 = 5, then divide by 10.  the exponent
   shouldn't matter?  clearly it must

   N = ((pow, base), digit) :: rest n

   N = base^pow * digit + (IH)  (digit is unknown)
   we just be greedy.  take the largest possible value that doesn't overflow.
   ^^ doesn't work
   just mod base^pow, recurse, digit is ez

   34 = x + 6y  (we're not using linear optimization for this)
   OK so running into issues here, we have to go top to bottom here.

   72 = 7x + y
*)
let rec smallest_repr_greedy n seq =
  if equal n 0 then []
  else
    let pow, base = Sequence.hd_exn seq in
    let digit = min ((n - (n % (base ** pow))) / (base ** pow)) 9 in
    let remainder = n - (digit * (base ** pow)) in
    (* can't have digit 10 - how can that logic come in? *)
    Stdio.printf "%d = %d * (%d ** %d) + remainder %d\n" n digit base pow
      remainder;
    digit :: smallest_repr_greedy remainder (Sequence.tl_eagerly_exn seq)

(* OK so this greedy approach isn't working.  I think we have to brute force? *)

let to_power_seq first len =
  let first_nums =
    Sequence.of_list @@ Sequence.to_list_rev
    @@ Sequence.take
         (Sequence.cycle_list_exn (List.rev (string_to_nums first)))
         len
  in
  Sequence.mapi first_nums ~f:(fun i n -> (len - 1 - i, n))

let%test_unit "to_power_seq" =
  [%test_eq: (int * int) list]
    [ (3, 3); (2, 2); (1, 1); (0, 0) ]
    (Sequence.to_list (to_power_seq "3210" 4))

let%test_unit "to_power_seq (2)" =
  [%test_eq: (int * int) list]
    [ (1, 7); (0, 8) ]
    (Sequence.to_list (to_power_seq "978" 2))

(* going backwards is right *)
let%test_unit "smallest_repr_greedy" =
  [%test_eq: int list] [ 5; 4 ]
    (smallest_repr_greedy 34 (Sequence.cycle_list_exn [ (1, 6); (0, 1) ]))

let%test_unit "smallest_repr_greedy (2)" =
  [%test_eq: int list] [ 9; 9 ]
    (smallest_repr_greedy 72 (Sequence.cycle_list_exn [ (1, 7); (0, 8) ]))

(* 1220 = 5^3 * 5 + 8^2 * 9 + 8 * 2 + 3 * 7^0 *)

(* let%test_unit "smallest_repr_greedy (3)" =
  [%test_eq: int list] [ 5; 9; 2; 3 ]
    (smallest_repr_greedy 1220
       (Sequence.cycle_list_exn [ (3, 5); (2, 8); (1, 8); (0, 7) ])) *)

let repr_to_int second_nums pow_seq =
  Sequence.zip pow_seq (Sequence.of_list second_nums)
  |> Sequence.map ~f:(fun ((pow, base), digit) -> (base ** pow) * digit)
  |> Sequence.fold ~init:0 ~f:( + )

let to_digits n = string_to_nums @@ Int.to_string n

(* deeply inefficient - can be much smarter with power_seq
   next issue: seq must be clamped to the length of our digits
 *)
let smallest_repr n ~target code =
  let curr = ref (n - 1) in
  let result = ref None in
  while !curr > 0 && Option.is_none !result do
    let curr_digits = to_digits !curr in
    let seq = to_power_seq code (List.length curr_digits) in
    if equal (repr_to_int curr_digits seq) target then result := Some !curr;
    curr := !curr - 1
  done;
  if Option.is_none !result then n else Option.value_exn !result

let%test_unit "smallest_repr (brute force)" =
  [%test_eq: int] 99 (smallest_repr 99 ~target:72 "978")

let%test_unit "smallest_repr (brute force 2)" =
  [%test_eq: int] 54 (smallest_repr 149 ~target:34 "61")

let smallest_repr_string start ~target code =
  Stdio.printf "---- smallest_repr_string %d ---\n" target;
  Int.to_string @@ smallest_repr start ~target code

let nums s =
  let is_iso_2 = String.is_substring s ~substring:"ISO2-" in
  match String.split ~on:' ' @@ List.nth_exn (String.split s ~on:'-') 1 with
  | [ first; second ] ->
      let second_nums = string_to_nums second in
      let len = List.length second_nums in
      let pow_seq = to_power_seq first len in
      (* this is silly *)
      let result = repr_to_int second_nums pow_seq in
      if
        (not is_iso_2)
        || equal_string
             (smallest_repr_string (Int.of_string second) ~target:result first)
             second
      then (
        Stdio.printf "accepting %d!\n" result;
        result)
      else (
        Stdio.printf "womp womp -- rejecting %d\n" result;
        0)
  | _ -> failwith "invalid"

let%test_unit "nums (easy)" = [%test_eq: int] 9 (nums "ISO-3 23")

let%test_unit "nums" =
  [%test_eq: int list] [ 222; 1252; 4 ]
    (List.map ~f:nums [ "ISO-3210 7654"; "ISO-8742 2468"; "ISO-1111 1111" ])

let%test_unit "nums - part2" =
  [%test_eq: int list]
    [ 222; 1252; 4; 77; 1416; 1158390; 102 ]
    (List.map ~f:nums
       [
         "ISO-3210 7654";
         "ISO-8742 2468";
         "ISO-1 1111";
         "ISO-13 777";
         "ISO-34 140856";
         "ISO-789 2465112";
         "ISO-2341 945";
       ])

let%test_unit "nums - part3" =
  [%test_eq: int list]
    [ 0; 0; 72; 1220; 38826; 10869; 0 ]
    (List.map ~f:nums
       [
         "ISO2-3 23";
         "ISO2-61 149";
         "ISO2-978 99";
         "ISO2-5887 5923";
         "ISO2-76988 123749";
         "ISO2-435657 929977";
         "ISO2-3949 870737";
       ])

let part1 l = List.fold ~f:( + ) ~init:0 @@ List.map ~f:nums l
let part2 = part1
let part3 = part1
