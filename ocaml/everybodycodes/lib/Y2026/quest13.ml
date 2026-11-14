[@@@ocaml.warning "-31-32"]

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

type clock = { hands : int; current : int; time_left : int; delta : int }
[@@deriving sexp, compare]

let show_clock clock =
  Printf.sprintf "{hands=%d; current=%d; delta=%d}" clock.hands clock.current
    clock.delta

let _ = show_clock

let rec parse_clocks hands direction current move =
  match (hands, direction, current, move) with
  | [], [], [], [] -> []
  | h :: hs, d :: ds, c :: cs, m :: ms ->
      { hands = h; current = c; time_left = m; delta = 1 * d }
      :: parse_clocks hs ds cs ms
  | _ -> invalid_arg "lists have different length"

let parse_lines l =
  match l with
  | [ l1; l2; l3; l4 ] ->
      let clocks =
        parse_clocks
          (parse_colons "hands %s" l1)
          (parse_directions l2)
          (parse_colons "current %s" l3)
          (parse_colons "move %s" l4)
      in
      let h =
        List.fold ~init:1 ~f:lcm (List.map ~f:(fun c -> c.hands) clocks)
      in
      List.map
        ~f:(fun clock ->
          {
            clock with
            hands = h;
            delta = clock.delta * h / clock.hands;
            current = clock.current * h / clock.hands;
          })
        clocks
  | _ -> failwith "invalid"

let p1_example1 =
  [ "hands 6:8:12"; "direction CW:CCW:CW"; "current 2:4:8"; "move 9:7:4" ]

let%test_unit "parse_lines" =
  [%test_result: clock list]
    ~expect:
      [
        { hands = 24; current = 8; time_left = 9; delta = 4 };
        { hands = 24; current = 12; time_left = 7; delta = -3 };
        { hands = 24; current = 16; time_left = 4; delta = 2 };
      ]
    (parse_lines p1_example1)

let is_done clock = equal_int clock.time_left 0

let tick clock =
  if is_done clock then clock
  else
    let next_current = clock.current + clock.delta in
    {
      clock with
      current = mod_positive next_current clock.hands;
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

let part1 _ = 1
(*
let%test_unit "part1 (first)" =
  [%test_result: int] ~expect:5 (part1 p1_example1)

let%test_unit "part1 (second)" =
  [%test_result: int] ~expect:7
    (part1
       [
         "hands 6:8:12"; "direction CW:CCW:CW"; "current 1:2:2"; "move 10:12:15";
       ])

let%test_unit "part1 (third)" =
  [%test_result: int] ~expect:9
    (part1
       [
         "hands 6:8:12"; "direction CW:CCW:CW"; "current 1:2:2"; "move 10:12:20";
       ]) *)

(* OK, so just think of the two hands as relative to each other, moving at an
   angular velocity *)

(* *)

let part2 _ = 1
let part3 _ = 1
