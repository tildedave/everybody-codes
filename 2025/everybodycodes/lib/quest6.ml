open Base

let char_locations s =
  snd
  @@ String.fold
       ~init:(0, Map.empty (module Char))
       ~f:(fun (n, m) ch ->
         ( n + 1,
           Map.update m ch ~f:(fun v ->
               match v with None -> [ n ] | Some l -> n :: l) ))
  @@ s

let num_pairs locations ch1 ch2 =
  Map.find_exn locations ch2
  |> List.cartesian_product (Map.find_exn locations ch1)
  |> List.filter ~f:(fun (mentor_n, novice_n) -> mentor_n < novice_n)
  |> List.length

let part1 s = num_pairs (char_locations s) 'A' 'a'
let%test_unit "part1 given" = [%test_eq: int] 5 (part1 "ABabACacBCbca")

let part2 s =
  let locations = char_locations s in
  num_pairs locations 'A' 'a'
  + num_pairs locations 'B' 'b'
  + num_pairs locations 'C' 'c'

let%test_unit "part2 given" = [%test_eq: int] 11 (part2 "ABabACacBCbca")

let num_pairs_part3 ~num_repeats ~distance ~length locations ch1 ch2 =
  Map.find_exn locations ch2
  |> List.cartesian_product (Map.find_exn locations ch1)
  |> List.map ~f:(fun (mentor_n, novice_n) ->
         (* primary case, mentor < novice and within the given distance *)
         (if Int.abs (novice_n - mentor_n) <= distance then num_repeats else 0)
         (* wraparound cases assume length of string * 2 < distance *)
         + (if
              (* wraparound case: |start| novice .... mentor |end| *)
              Int.abs (novice_n - mentor_n - length) <= distance
            then num_repeats - 1
            else 0)
         +
         if
           (* wraparound case: |start| mentor .... novice |end| *)
           Int.abs (mentor_n - novice_n - length) <= distance
         then num_repeats - 1
         else 0)
  |> Util.sum_list

let num_pairs_brute_force ~distance locations ch1 ch2 =
  Map.find_exn locations ch2
  |> List.cartesian_product (Map.find_exn locations ch1)
  |> List.filter ~f:(fun (mentor_n, novice_n) ->
         Int.abs (mentor_n - novice_n) <= distance)
  |> List.length

let part3 s ~distance ~num_repeats =
  let locations = char_locations s in
  num_pairs_part3 ~distance ~num_repeats ~length:(String.length s) locations 'A'
    'a'
  + num_pairs_part3 ~distance ~num_repeats ~length:(String.length s) locations
      'B' 'b'
  + num_pairs_part3 ~distance ~num_repeats ~length:(String.length s) locations
      'C' 'c'

let rec repeat s n = if equal n 1 then s else String.append s (repeat s (n - 1))

let part3_brute_force s ~distance ~num_repeats =
  let locations = char_locations (repeat s num_repeats) in
  num_pairs_brute_force ~distance locations 'A' 'a'
  + num_pairs_brute_force ~distance locations 'B' 'b'
  + num_pairs_brute_force ~distance locations 'C' 'c'

let%test_unit "part3 given (1)" =
  [%test_eq: int] 34
    (part3 "AABCBABCABCabcabcABCCBAACBCa" ~distance:10 ~num_repeats:1)

let%test_unit "part3 given (1)" =
  [%test_eq: int] 34
    (part3_brute_force "AABCBABCABCabcabcABCCBAACBCa" ~distance:10
       ~num_repeats:1)

let%test_unit "part3 given (2)" =
  [%test_eq: int] 72
    (part3 "AABCBABCABCabcabcABCCBAACBCa" ~distance:10 ~num_repeats:2)

let%test_unit "part3 given (2)" =
  [%test_eq: int] 72
    (part3_brute_force "AABCBABCABCabcabcABCCBAACBCa" ~distance:10
       ~num_repeats:2)

let%test_unit "part3 given (3)" =
  [%test_eq: int] 110
    (part3 "AABCBABCABCabcabcABCCBAACBCa" ~distance:10 ~num_repeats:3)

let%test_unit "part3 given (3)" =
  [%test_eq: int] 110
    (part3_brute_force "AABCBABCABCabcabcABCCBAACBCa" ~distance:10
       ~num_repeats:3)

let%test_unit "part3 given (4)" =
  [%test_eq: int] 148
    (part3 "AABCBABCABCabcabcABCCBAACBCa" ~distance:10 ~num_repeats:4)

let%test_unit "part3 given (4)" =
  [%test_eq: int] 148
    (part3_brute_force "AABCBABCABCabcabcABCCBAACBCa" ~distance:10
       ~num_repeats:4)

(* incorrect *)
(*
let%test_unit "part3 given (3)" =
  [%test_eq: int] 3442321
    (part3 "AABCBABCABCabcabcABCCBAACBCa" ~distance:1000 ~num_repeats:1000)
*)
