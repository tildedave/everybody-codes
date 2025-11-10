open Base

(* val eni: int -> int -> int -> ?only:int; *)

let eni ?only:take_opt n exp m =
  let rec _eni score l exp =
    match exp with
    | 0 -> l
    | _ ->
        let next = score * n % m in
        _eni next (next :: l) (exp - 1)
  in
  Util.concat_ints
  @@
  match take_opt with
  | None -> _eni 1 [] exp
  | Some n -> List.take (_eni 1 [] exp) n

let%test_unit "part1 (eni)" = [%test_eq: int] 1342 (eni 2 4 5)
let%test_unit "part1 (eni 2) " = [%test_eq: int] 311193 (eni 3 5 16)
let%test_unit "part2 (eni)" = [%test_eq: int] 34213 (eni ~only:5 2 7 5)
let%test_unit "part2 (eni 2)" = [%test_eq: int] 111931 (eni ~only:5 3 8 16)

let eni_line ?only:take_opt s =
  let e = eni ?only:take_opt in
  Stdlib.Scanf.sscanf s "A=%d B=%d C=%d X=%d Y=%d Z=%d M=%d"
    (fun a b c x y z m -> e a x m + e b y m + e c z m)

let highest_number ?only:take_opt l =
  Option.value_exn
  @@ List.max_elt (List.map ~f:(eni_line ?only:take_opt) l) ~compare:compare_int

let quest1part1 = highest_number ?only:None
let quest1part2 = highest_number ?only:(Some 5)

let%test_unit "quest 1 part1 (given)" =
  [%test_eq: int] 11611972920
    (quest1part1
       [
         "A=4 B=4 C=6 X=3 Y=4 Z=5 M=11";
         "A=8 B=4 C=7 X=8 Y=4 Z=6 M=12";
         "A=2 B=8 C=6 X=2 Y=4 Z=5 M=13";
         "A=5 B=9 C=6 X=8 Y=6 Z=8 M=14";
         "A=5 B=9 C=7 X=6 Y=6 Z=8 M=15";
         "A=8 B=8 C=8 X=6 Y=9 Z=6 M=16";
       ])

let%test_unit "quest 1 part2 (given)" =
  [%test_eq: int] 11051340
    (quest1part2
       [
         "A=4 B=4 C=6 X=3 Y=14 Z=15 M=11";
         "A=8 B=4 C=7 X=8 Y=14 Z=16 M=12";
         "A=2 B=8 C=6 X=2 Y=14 Z=15 M=13";
         "A=5 B=9 C=6 X=8 Y=16 Z=18 M=14";
         "A=5 B=9 C=7 X=6 Y=16 Z=18 M=15";
         "A=8 B=8 C=8 X=6 Y=19 Z=16 M=16";
       ])
(*
let%test_unit "quest 1 part2 (given 2)" =
  [%test_eq: int] 11051340
    (quest1part2
       [
         "A=3657 B=3583 C=9716 X=903056852 Y=9283895500 Z=85920867478 M=188";
         "A=6061 B=4425 C=5082 X=731145782 Y=1550090416 Z=87586428967 M=107";
         "A=7818 B=5395 C=9975 X=122388873 Y=4093041057 Z=58606045432 M=102";
         "A=7681 B=9603 C=5681 X=716116871 Y=6421884967 Z=66298999264 M=196";
         "A=7334 B=9016 C=8524 X=297284338 Y=1565962337 Z=86750102612 M=145";
       ]) *)
