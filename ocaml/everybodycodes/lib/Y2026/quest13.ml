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

type clock = {
  hands : int;
  direction : int;
  current : int;
  time_left : int;
  winding_number : int;
}
[@@deriving sexp, compare]

let show_clock clock =
  Printf.sprintf "{hands=%d; current=%d; direction=%d}" clock.hands
    clock.current clock.direction

let _ = show_clock

let rec parse_clocks hands direction current move =
  match (hands, direction, current, move) with
  | [], [], [], [] -> []
  | h :: hs, d :: ds, c :: cs, m :: ms ->
      {
        hands = h;
        direction = d;
        current = c;
        time_left = m;
        winding_number = 0;
      }
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

let p1_example1 =
  [ "hands 6:8:12"; "direction CW:CCW:CW"; "current 2:4:8"; "move 9:7:4" ]

let%test_unit "parse_lines" =
  [%test_result: clock list]
    ~expect:
      [
        {
          hands = 6;
          direction = 1;
          current = 2;
          time_left = 9;
          winding_number = 0;
        };
        {
          hands = 8;
          direction = -1;
          current = 4;
          time_left = 7;
          winding_number = 0;
        };
        {
          hands = 12;
          direction = 1;
          current = 8;
          time_left = 4;
          winding_number = 0;
        };
      ]
    (parse_lines p1_example1)

let is_done clock = equal_int clock.time_left 0

let tick clock =
  if is_done clock then clock
  else
    let next_current = clock.current + clock.direction in
    let wno =
      if next_current >= clock.hands then clock.winding_number + 1
      else if next_current < 0 then clock.winding_number - 1
      else clock.winding_number
    in
    {
      clock with
      current = mod_positive next_current clock.hands;
      time_left = clock.time_left - 1;
      winding_number = wno;
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

(* problem: need to figure out how to handle "pass 0" calculation *)
(* this shouldn't be too bad *)
(* can calculate the angle between them cos theta = (a * b / |a||b|) *)
(* these are all roots of unity *)
(*
let clock_to_complex clock =
  Stdlib.Complex.polar 1.0 (clock.current // clock.hands)

(* view c1 as a 2d vector, so c1 * c2 = c1.re * c2.re + c1.im * c1.im * *)

(* will return radians between 0 and pi *)

(* of course this is symmetric so can't tell which is on top of the other *)

let angle_between (c1 : Stdlib.Complex.t) (c2 : Stdlib.Complex.t) =
  Stdlib.acos ((c1.re *. c2.re) +. (c1.im *. c2.im))

let example_clock = { hands = 12; current = 10; direction = 1; time_left = 0 }

let%test_unit "angle_between" =
  [%test_result: float list] ~expect:[]
    [
      angle_between
        (clock_to_complex example_clock)
        (clock_to_complex { example_clock with current = 2 });
      angle_between
        (clock_to_complex { example_clock with current = 2 })
        (clock_to_complex example_clock);
    ] *)

(* z = e^(2pi clock.current / clock.hands) *)
(* e.g. arg = 2pi current / hands *)

(* let to_interval prev next =
  assert (equal_int prev.hands next.hands);
  let open Bignum in
  let h = of_int prev.hands in
  let p, n = (of_int prev.current / h, of_int next.current / h) in
  (min p n, max p n)
*)

(* let overlaps (lo, hi) (lo2, hi2) =
  let open Bignum in
  not (hi < lo2 || lo > hi2) *)

(* let norm_to_zero_one b =
  let open Bignum in
  let r = ref b in
  while !r < zero do
    r := !r + one
  done;
  while !r > one do
    r := !r - one
  done;
  !r *)

(* let in_interval x (lo, hi) =
  let open Bignum in
  lo <= x && x <= hi *)

(* let show_interval (lo, hi) =
  Printf.sprintf "(%s, %s)"
    (Bignum.to_string_accurate lo)
    (Bignum.to_string_accurate hi)

let clock_to_bignum c =
  let open Bignum in
  of_int c.current / of_int c.hands

let clock_to_interval c c_next = (clock_to_bignum c, clock_to_bignum c_next)

let timepair_to_intervals (start_c, end_c) =
  match
    Ordering.of_int (compare start_c.winding_number end_c.winding_number)
  with
  | Equal -> [ clock_to_interval start_c end_c ]
  | Less ->
      [
        (clock_to_bignum start_c, Bignum.one);
        (Bignum.zero, clock_to_bignum end_c);
      ]
  | Greater ->
      [
        (clock_to_bignum end_c, Bignum.one);
        (Bignum.zero, clock_to_bignum start_c);
      ] *)

(* their example: beige --> 1 = hands 6, green --> 2 = hands 8, red --> 3 = hands 12 *)

(* let crossed c start_c end_c =
  assert (equal_int start_c.direction end_c.direction);
  (* case 1: start_c --> end_c, no winding number change *)
  let bn_clock = clock_to_bignum c in
  if equal_int start_c.winding_number end_c.winding_number then
    in_interval bn_clock (clock_to_interval start_c end_c)
  else if start_c.winding_number < end_c.winding_number then
    (* we wound across the clock, e.g. 10pm --> 2am *)
    (* (potentially many times, but I'm not sure if that matters) *)
    in_interval bn_clock (clock_to_bignum start_c, Bignum.one)
    || in_interval bn_clock (Bignum.zero, clock_to_bignum end_c)
  else (* we wound across the clock backwards e.g. 2am --> 10pm *)
    in_interval bn_clock (Bignum.zero, clock_to_bignum start_c)
    || in_interval bn_clock (clock_to_bignum end_c, Bignum.one) *)

(* case 2: start_c --> end_c, winding number change, heading CW *)
(* case 3: start_c --> end_c, winding number change, heading CCW *)

(* OK, another way to think about this is that the clocks are modeled by e^2pi/h t *)
(* solve for all possible t's *)

let paths_overlap clock1 clock2 =
  let k1, n1, k2, n2 =
    (clock1.current, clock1.hands, clock2.current, clock2.hands)
  in
  let c1, c2 = (clock1.direction, clock2.direction) in
  (* special case 0 in the case that the clocks are moving in the opposite direction *)
  (* OK 0 special case is messing up my 0 --> 1 1 <-- 0 detection *)
  (* let k1, k2 =
    match (c1, c2) with
    | -1, -1 -> (k1, k2)
    | 1, 1 -> (k1, k2)
    | -1, 1 -> ((if equal_int k1 1 then k1 + n1 else k1), k2)
    | 1, -1 -> (k1, if equal_int k2 1 then k2 + n2 else k2)
    | _ -> invalid_arg "invalid direction"
  in *)
  (* I need to just sit down and work through this - this is not working *)
  let k1, k2 =
    ( (if equal_int clock1.current 1 && equal_int clock1.direction (-1) then
         k1 + n1
       else k1),
      if equal_int clock2.current 1 && equal_int clock2.direction (-1) then
        k2 + k2
      else k2 )
  in
  let n, d = ((n2 * k1) - (n1 * k2), (c2 * n1) - (c1 * n2)) in
  let open Bignum in
  let f = of_int n / of_int d in
  Stdio.printf "f: %s\n" (Bignum.to_string_accurate f);
  zero < f && f <= one

let test_clock =
  { hands = 12; current = 0; time_left = 0; winding_number = 0; direction = 1 }

let%test_unit "intersections near origin" =
  [%test_result: bool list] ~expect:[ true; false; true ]
    (List.map
       ~f:(fun (c1, c2) -> paths_overlap c1 c2)
       [
         ( { test_clock with current = 1; direction = -1 },
           { test_clock with current = 0; direction = 1 } );
         ( { test_clock with current = 0; direction = -1 },
           { test_clock with current = 0; direction = 1 } );
         ( { test_clock with current = 11; direction = 1 },
           { test_clock with current = 0; direction = -1 } );
       ])

let signals clocks =
  Sequence.unfold ~init:clocks ~f:(fun clocks ->
      if List.for_all ~f:is_done clocks then None
      else
        let num_signals =
          List.fold ~init:0
            ~f:(fun acc (clock1, clock2) ->
              if paths_overlap clock1 clock2 then
                (* Stdio.printf "\tpaths overlap %s -> %s\n" (show_clock clock1)
                  (show_clock clock2); *)
                acc + 1
              else
                (* Stdio.printf "\tno overlap %s -> %s\n" (show_clock clock1)
                  (show_clock clock2); *)
                acc)
            (* 1 was previously at current/hands, then goes to current / hands
              (* can treat this as an interval I guess.  no more mods means we're
           continuous *)
              (* let i1 = to_interval prev1 next1 in
              let i2 = to_interval prev2 next2 in *)
              (* this is not correct since we have some issues near the origin *)
              if overlaps i1 i2 then (
                Stdio.printf "i1 = %s\n" (show_interval i1);
                Stdio.printf "i2 = %s\n" (show_interval i2);
                acc + 1) *)
            (all_pairs clocks)
        in
        Some (num_signals, List.map ~f:tick clocks))

(* if clocks pass each other or stop in the same place (but were all still
       moving), emit signal *)

let part1 l = Sequence.sum (module Int) (signals (parse_lines l)) ~f:Fn.id

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
       ])

let part2 _ = 1
let part3 _ = 1
