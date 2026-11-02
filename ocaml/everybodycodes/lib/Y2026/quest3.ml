open Base

let string_to_nums s =
  s
  |> String.fold ~init:[] ~f:(fun acc ch ->
      (Int.of_string @@ Char.to_string ch) :: acc)
  |> List.rev

let%test_unit "string_to_nums" =
  [%test_eq: int list] [ 3; 4; 5; 2 ] (string_to_nums "3452")

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

let repr_to_int second pow_seq =
  Sequence.zip pow_seq (String.to_sequence second)
  |> Sequence.map ~f:(fun ((pow, base), ch) ->
      (base ** pow) * Int.of_string (Char.to_string ch))
  |> Sequence.fold ~init:0 ~f:( + )

let to_digits n = string_to_nums @@ Int.to_string n

(* just brute force it *)
let smallest_repr n ~target code =
  let curr = ref (n - 1) in
  let n_length = List.length (to_digits n) in
  let pow_seq = to_power_seq code n_length in
  let result = ref None in
  while !curr > 0 && Option.is_none !result do
    let curr_string = Int.to_string !curr in
    (* if length (curr_digits) < n_length for the powers, we drop the first n *)
    let seq = Sequence.drop pow_seq (n_length - String.length curr_string) in
    if equal (repr_to_int curr_string seq) target then result := Some !curr;
    (* seems clear that there is a point where we could stop, but this is fast enough I guess *)
    curr := !curr - 1
  done;
  if Option.is_none !result then n else Option.value_exn !result

let%test_unit "smallest_repr (brute force)" =
  [%test_eq: int] 99 (smallest_repr 99 ~target:72 "978")

let%test_unit "smallest_repr (brute force 2)" =
  [%test_eq: int] 54 (smallest_repr 149 ~target:34 "61")

let smallest_repr_string start ~target code =
  Int.to_string @@ smallest_repr start ~target code

let nums s =
  let is_iso_2 = String.is_substring s ~substring:"ISO2-" in
  match String.split ~on:' ' @@ List.nth_exn (String.split s ~on:'-') 1 with
  | [ first; second ] ->
      let len = String.length second in
      let pow_seq = to_power_seq first len in
      let result = repr_to_int second pow_seq in
      if
        (not is_iso_2)
        || equal_string
             (smallest_repr_string (Int.of_string second) ~target:result first)
             second
      then result
      else 0
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
