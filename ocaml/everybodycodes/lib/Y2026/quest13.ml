open Base
open Util

(* let _ = Bignum.of_int 1 / Bignum.of_int 3 *)

(*
 Every moving hand advances by exactly one of its positions per second.
 Its movement between two positions is smooth, so two hands may meet at any
 moment during that second, not only when they reach their next positions.
*)

(* OK it seems like we can use fractions here *)

let parse_colons fmt s =
  Stdlib.Scanf.sscanf s fmt (fun s ->
      List.map ~f:Int.of_string (String.split ~on:':' s))

let parse_directions s =
  Stdlib.Scanf.sscanf s "direction %s" (fun s ->
      List.map
        ~f:(function
          | "CW" -> 1 | "CCW" -> -1 | _ -> invalid_arg "invalid direction")
        (String.split ~on:':' s))

let%test_unit "parse_colons" =
  [%test_result: int list] ~expect:[ 6; 8; 12 ]
    (parse_colons "hands %s" "hands 6:8:12")

type clock = { hands : int; direction : int; current : int; time_left : int }
[@@deriving sexp, compare]

let rec parse_clocks hands direction current move =
  match (hands, direction, current, move) with
  | [], [], [], [] -> []
  | h :: hs, d :: ds, c :: cs, m :: ms ->
      { hands = h; direction = d; current = c; time_left = m }
      :: parse_clocks hs ds cs ms
  | _ -> invalid_arg "lists have different length"

let parse_lines l =
  match l with
  | [ l1; l2; l3; l4 ] ->
      parse_clocks
        (parse_colons "hands %s" l1)
        (parse_directions l2)
        (parse_colons "current %s" l3)
        (parse_colons "move %s" l4)
  | _ -> failwith "invalid"

let example =
  [ "hands 6:8:12"; "direction CW:CCW:CW"; "current 2:4:8"; "move 9:7:4" ]

let%test_unit "parse_lines" =
  [%test_result: clock list]
    ~expect:
      [
        { hands = 6; direction = 1; current = 2; time_left = 9 };
        { hands = 8; direction = -1; current = 4; time_left = 7 };
        { hands = 12; direction = 1; current = 8; time_left = 4 };
      ]
    (parse_lines example)

let is_done clock = equal_int clock.time_left 0

let tick clock =
  if is_done clock then clock
  else
    {
      clock with
      current = mod_positive (clock.current + clock.direction) clock.hands;
      time_left = clock.time_left - 1;
    }

let pair i x = (i, x)

let all_pairs l =
  List.filter_map
    ~f:(fun ((i, x), (j, y)) ->
      if equal i j then None else if i > j then None else Some (x, y))
    (List.cartesian_product (List.mapi ~f:pair l) (List.mapi ~f:pair l))

let%test_unit "all_pairs" =
  [%test_result: (int * int) list]
    ~expect:[ (1, 2); (1, 3); (2, 3) ]
    (all_pairs [ 1; 2; 3 ])

let signals clocks =
    let prev, next = List.map ~f:(fun clock -> (clock, tick clock)) clocks in
    (* if clocks pass each other or stop in the same place (but were all still
       moving), emit signal *)

let part1 _ = 1
let part2 _ = 1
let part3 _ = 1
