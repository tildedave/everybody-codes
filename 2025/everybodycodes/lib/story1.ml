open Base
open Util

(* val eni: int -> int -> int -> ?only:int; *)

let rec _eni score l exp n m =
  match exp with
  | 0 -> l
  | _ ->
      let next = score * n % m in
      _eni next (next :: l) (exp - 1) n m

let eni ?only:take_opt n exp m =
  Util.concat_ints
  @@
  match take_opt with
  | None -> _eni 1 [] exp n m
  | Some s ->
      let lop_off = exp - s in
      if lop_off >= 0 then _eni (mod_exp n lop_off m) [] s n m
      else _eni 1 [] exp n m

let%test_unit "part1 (eni)" = [%test_eq: int] 1342 (eni 2 4 5)
let%test_unit "part1 (eni 2) " = [%test_eq: int] 311193 (eni 3 5 16)
let%test_unit "part2 (eni)" = [%test_eq: int] 34213 (eni ~only:5 2 7 5)
let%test_unit "part2 (eni 2)" = [%test_eq: int] 111931 (eni ~only:5 3 8 16)

let eni_line e s =
  Stdlib.Scanf.sscanf s "A=%d B=%d C=%d X=%d Y=%d Z=%d M=%d"
    (fun a b c x y z m -> e a x m + e b y m + e c z m)

let highest_number eni_line_fn l =
  Option.value_exn
  @@ List.max_elt (List.map ~f:eni_line_fn l) ~compare:compare_int

let quest1part1 = highest_number (eni_line (eni ?only:None))
let quest1part2 = highest_number (eni_line (eni ?only:(Some 5)))

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

let%test_unit "quest 1 part2 (given 2)" =
  [%test_eq: int] 1507702060886
    (quest1part2
       [
         "A=3657 B=3583 C=9716 X=903056852 Y=9283895500 Z=85920867478 M=188";
         "A=6061 B=4425 C=5082 X=731145782 Y=1550090416 Z=87586428967 M=107";
         "A=7818 B=5395 C=9975 X=122388873 Y=4093041057 Z=58606045432 M=102";
         "A=7681 B=9603 C=5681 X=716116871 Y=6421884967 Z=66298999264 M=196";
         "A=7334 B=9016 C=8524 X=297284338 Y=1565962337 Z=86750102612 M=145";
       ])

let rec _residues_part3 score l n m ~seen =
  let next = score * n % m in
  if Set.mem seen next then
    List.split_while ~f:(fun x -> not (equal x next)) (List.rev l)
  else _residues_part3 next (next :: l) n m ~seen:(Set.add seen next)

let eni_part3 n exp m =
  let start, residue_list =
    _residues_part3 1 [] n m ~seen:(Base.Set.empty (module Int))
  in
  let start_len, residue_len = (List.length start, List.length residue_list) in
  if exp < start_len then sum_list (List.take start exp)
  else
    let num_repeats, overflow =
      ((exp - start_len) / residue_len, (exp - start_len) % residue_len)
    in
    sum_list start
    + (sum_list residue_list * num_repeats)
    + sum_list (List.take residue_list overflow)

let%test_unit "part3 (eni)" = [%test_eq: int] 19 (eni_part3 2 7 5)
let%test_unit "part3 (eni 2) " = [%test_eq: int] 48 (eni_part3 3 8 16)
let%test_unit "part3 (eni 3)" = [%test_eq: int] 132000 (eni_part3 4 3000 110)
let%test_unit "part3 (eni 4)" = [%test_eq: int] 616000 (eni_part3 4 14000 110)
let%test_unit "part3 (eni 5)" = [%test_eq: int] 825000 (eni_part3 6 15000 110)
let%test_unit "part3 (eni 6)" = [%test_eq: int] 559940 (eni_part3 4 14000 120)
let%test_unit "part3 (eni 7)" = [%test_eq: int] 910000 (eni_part3 8 14000 130)
let%test_unit "part3 (eni 8)" = [%test_eq: int] 479880 (eni_part3 8 6000 160)
let%test_unit "part3 (eni 9)" = [%test_eq: int] 1519880 (eni_part3 8 19000 160)
let%test_unit "part3 (eni 10)" = [%test_eq: int] 1279880 (eni_part3 8 16000 160)

(* so everything repeats eventually, we can *)

let quest1part3 = highest_number (eni_line eni_part3)

let%test_unit "quest 1 part3 (given)" =
  [%test_eq: int] 3279640
    (quest1part3
       [
         "A=4 B=4 C=6 X=3000 Y=14000 Z=15000 M=110";
         "A=8 B=4 C=7 X=8000 Y=14000 Z=16000 M=120";
         "A=2 B=8 C=6 X=2000 Y=14000 Z=15000 M=130";
         "A=5 B=9 C=6 X=8000 Y=16000 Z=18000 M=140";
         "A=5 B=9 C=7 X=6000 Y=16000 Z=18000 M=150";
         "A=8 B=8 C=8 X=6000 Y=19000 Z=16000 M=160";
       ])

let%test_unit "quest 1 part3 (given 2)" =
  [%test_eq: int] 7276515438396
    (quest1part3
       [
         "A=3657 B=3583 C=9716 X=903056852 Y=9283895500 Z=85920867478 M=188";
         "A=6061 B=4425 C=5082 X=731145782 Y=1550090416 Z=87586428967 M=107";
         "A=7818 B=5395 C=9975 X=122388873 Y=4093041057 Z=58606045432 M=102";
         "A=7681 B=9603 C=5681 X=716116871 Y=6421884967 Z=66298999264 M=196";
         "A=7334 B=9016 C=8524 X=297284338 Y=1565962337 Z=86750102612 M=145";
       ])

type tree = Leaf | Node of (int * string) * tree * tree

let rec tree_insert tree (n, label) =
  match tree with
  | Leaf -> Node ((n, label), Leaf, Leaf)
  | Node (d, left, right) ->
      if n < fst d then Node (d, tree_insert left (n, label), right)
      else if n > fst d then Node (d, left, tree_insert right (n, label))
      else failwith "duplicate inserted"

let process_line (left_tree, right_tree) s =
  Stdlib.Scanf.sscanf s "ADD id=%d left=[%d,%c] right=[%d,%c]"
    (fun _ ln llabel rn rlabel ->
      ( tree_insert left_tree (ln, Char.to_string llabel),
        tree_insert right_tree (rn, Char.to_string rlabel) ))

(* let rec nodes_per_level tree n =
   match (tree, n) with
   | Leaf, _ -> 0
   | _, 0 -> 1
   | Node (_, left, right), n ->
       nodes_per_level left (n - 1) + nodes_per_level right (n - 1) *)

let rec height tree =
  match tree with
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (height left) (height right)

let rec message_per_level tree n =
  match (tree, n) with
  | Leaf, _ -> ""
  | Node ((_, s), _, _), 0 -> s
  | Node (_, left, right), n ->
      String.append
        (message_per_level left (n - 1))
        (message_per_level right (n - 1))

let max_message_per_tree tree =
  let tree_height = height tree in
  0 -- (tree_height - 1)
  |> List.map ~f:(message_per_level tree)
  |> List.max_elt ~compare:(fun s1 s2 ->
         compare_int (String.length s1) (String.length s2))
  |> Option.value_exn

let quest2part1 l =
  let left, right = List.fold ~f:process_line ~init:(Leaf, Leaf) l in
  String.append (max_message_per_tree left) (max_message_per_tree right)

let%test_unit "quest1part1 (given, part 1)" =
  [%test_eq: string] "CFGNLK"
    (quest2part1
       [
         "ADD id=1 left=[10,A] right=[30,H]";
         "ADD id=2 left=[15,D] right=[25,I]";
         "ADD id=3 left=[12,F] right=[31,J]";
         "ADD id=4 left=[5,B] right=[27,L]";
         "ADD id=5 left=[3,C] right=[28,M]";
         "ADD id=6 left=[20,G] right=[32,K]";
         "ADD id=7 left=[4,E] right=[21,N]";
       ])

let%test_unit "quest1part1 (given, part 2)" =
  [%test_eq: string] "EVERYBODYCODES"
    (quest2part1
       [
         "ADD id=1 left=[160,E] right=[175,S]";
         "ADD id=2 left=[140,W] right=[224,D]";
         "ADD id=3 left=[122,U] right=[203,F]";
         "ADD id=4 left=[204,N] right=[114,G]";
         "ADD id=5 left=[136,V] right=[256,H]";
         "ADD id=6 left=[147,G] right=[192,O]";
         "ADD id=7 left=[232,I] right=[154,K]";
         "ADD id=8 left=[118,E] right=[125,Y]";
         "ADD id=9 left=[102,A] right=[210,D]";
         "ADD id=10 left=[183,Q] right=[254,E]";
         "ADD id=11 left=[146,E] right=[148,C]";
         "ADD id=12 left=[173,Y] right=[299,S]";
         "ADD id=13 left=[190,B] right=[277,B]";
         "ADD id=14 left=[124,T] right=[142,N]";
         "ADD id=15 left=[153,R] right=[133,M]";
         "ADD id=16 left=[252,D] right=[276,M]";
         "ADD id=17 left=[258,I] right=[245,P]";
         "ADD id=18 left=[117,O] right=[283,!]";
         "ADD id=19 left=[212,O] right=[127,R]";
         "ADD id=20 left=[278,A] right=[169,C]";
       ])
