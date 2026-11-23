open Base
open Util

type color = Black | Cyan | Magenta | Yellow | Green | Red | Blue
[@@deriving equal, compare, sexp_of]

module Color = struct
  type t = color [@@deriving compare, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let combine_color col1 col2 =
  match (col1, col2) with
  | Black, _ | _, Black -> Black
  | Cyan, Magenta | Magenta, Cyan -> Blue
  | Cyan, Yellow | Yellow, Cyan -> Green
  | Magenta, Yellow | Yellow, Magenta -> Red
  | Blue, _ | _, Blue -> invalid_arg "invalid blue argument"
  | Red, _ | _, Red -> invalid_arg "invalid red argument"
  | Green, _ | _, Green -> invalid_arg "invalid green argument"
  | _, _ ->
      assert (equal_color col1 col2);
      col1

let parse_color ch =
  match ch with
  | 'C' -> Cyan
  | 'M' -> Magenta
  | 'Y' -> Yellow
  | 'K' -> Black
  | _ -> invalid_arg "invalid color"

let num_crystals m =
  (* count the frequency of the data *)
  m |> Map.to_sequence
  |> Sequence.map ~f:(fun (_, color) -> (color, 1))
  |> Map.of_sequence_reduce (module Color) ~f:( + )
  |> Map.to_alist
  |> List.sort ~compare:(Comparable.lift Int.descending ~f:snd)
  |> fun l -> List.take l 3 |> List.map ~f:snd |> List.fold ~init:1 ~f:( * )

let combine_grid g1 g2 =
  assert (equal_tuple g1.bounds g2.bounds);
  let m1 = Map.of_alist_exn (module IntPair_Comparator) (grid_to_alist g1) in
  let m2 = Map.of_alist_exn (module IntPair_Comparator) (grid_to_alist g2) in
  Map.merge m1 m2 ~f:(fun ~key:_ merge_ele ->
      match merge_ele with
      | `Both (ch1, ch2) ->
          Some (combine_color (parse_color ch1) (parse_color ch2))
      | _ -> invalid_arg "must have both")

let part1 l =
  let first, next = List.split_while l ~f:(Fn.non String.is_empty) in
  combine_grid (to_grid first) (to_grid (List.tl_exn next)) |> num_crystals

let example =
  [
    "CCCCC";
    "MMMMM";
    "YYYYY";
    "KKKKK";
    "CCMYY";
    "";
    "CMYKC";
    "CMYKC";
    "CMYKC";
    "CMYKC";
    "CMYKC";
  ]

let%test_unit "part1" = [%test_result: int] ~expect:144 (part1 example)

(* part2 is mostly the same as part1 *)
(* 24 possible assignments, I could just hardcode it *)

let all_mappings =
  List.map
    ~f:((Fn.flip List.zip_exn) [ Black; Cyan; Yellow; Magenta ])
    (permutations [ 1; 2; 3; 4 ])

let combine_p2 g1 g2 m =
  assert (equal_tuple g1.bounds g2.bounds);
  let m1 = Map.of_alist_exn (module IntPair_Comparator) (grid_to_alist g1) in
  let m2 = Map.of_alist_exn (module IntPair_Comparator) (grid_to_alist g2) in
  Map.merge m1 m2 ~f:(fun ~key:_ merge_ele ->
      match merge_ele with
      | `Both (ch1, ch2) ->
          Some
            (combine_color (parse_color ch1) (Map.find_exn m (char_to_int ch2)))
      | _ -> invalid_arg "must have both")

let part2 l =
  let first, next = List.split_while l ~f:(Fn.non String.is_empty) in
  List.map
    ~f:(fun l ->
      Map.of_alist_exn (module Int) l
      |> combine_p2 (to_grid first) (to_grid (List.tl_exn next))
      |> num_crystals)
    all_mappings
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

(* combine_p2 (to_grid first) (to_grid (List.tl_exn next)) *)

let p2_example =
  [
    "CCCCC";
    "MMMMM";
    "YYYYY";
    "KKKKK";
    "CCMYY";
    "";
    "14231";
    "14231";
    "14231";
    "14231";
    "14231";
  ]

let%test_unit "part2" = [%test_result: int] ~expect:180 (part2 p2_example)
let part3 _ = 1
