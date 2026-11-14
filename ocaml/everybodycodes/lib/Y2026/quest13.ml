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
      {
        hands = h;
        current = c;
        time_left = m;
        delta = (if equal_int m 0 then 0 else 1 * d);
      }
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
      delta = (if equal_int clock.time_left 1 then 0 else clock.delta);
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

let clocks_overlap clock other_clock =
  (* OK think of movement relative to the first clock *)
  (* clock velocity = 0 -> and positive velocity is CW (say)
     other clock velocity = *)
  (* angle_diff = distance, velocity = distance / second,
     if velocity * 1 second >= angle_diff then overlap *)
  let angle_diff = other_clock.current - clock.current in
  if equal_int angle_diff 0 then
    false (* we counted this as an intersection on the previous cycle *)
  else
    let velocity = other_clock.delta - clock.delta in
    (* in 1 second, we travel velocity angle units. *)
    (* normalize angle_diff relative to the clock *)
    let angle_diff =
      if angle_diff < 0 then angle_diff + clock.hands else angle_diff
    in
    angle_diff + velocity <= 0 || angle_diff + velocity >= clock.hands

let%test_unit "clocks_overlap (overtake)" =
  [%test_result: bool] ~expect:true
    (clocks_overlap
       { hands = 4; current = 1; delta = 2; time_left = 0 }
       { hands = 4; current = 2; delta = 1; time_left = 0 })

let%test_unit
    "clocks_overlap (main clock moving away, they meet at the other end)" =
  [%test_result: bool] ~expect:true
    (clocks_overlap
       { hands = 4; current = 1; delta = -2; time_left = 0 }
       { hands = 4; current = 2; delta = 1; time_left = 0 })

let%test_unit
    "clocks_overlap (main clock moving away, they cross at the other end)" =
  [%test_result: bool] ~expect:true
    (clocks_overlap
       { hands = 5; current = 1; delta = -3; time_left = 0 }
       { hands = 5; current = 2; delta = 2; time_left = 0 })

let%test_unit "clocks_overlap (meet at center, wrap around)" =
  [%test_result: bool] ~expect:true
    (clocks_overlap
       { hands = 24; current = 3; delta = -3; time_left = 0 }
       { hands = 24; current = 22; delta = 2; time_left = 0 })

let%test_unit "clocks_overlap (they don't meet, moving away from each other)" =
  [%test_result: bool] ~expect:false
    (clocks_overlap
       { hands = 24; current = 12; delta = 4; time_left = 0 }
       { hands = 24; current = 9; delta = -3; time_left = 0 })

(*
    ~expect:[ true; false; true; false; true ]
    [
      ;
      clocks_overlap
        { hands = 4; current = 1; delta = 2; time_left = 0 }
        { hands = 4; current = 2; delta = -1; time_left = 0 };
      clocks_overlap
        ;
    ] *)

let signals clocks =
  Sequence.unfold ~init:clocks ~f:(fun clocks ->
      (* Stdio.printf "---- TICK ----\n"; *)
      if List.for_all ~f:is_done clocks then None
      else
        let num_signals =
          List.fold ~init:0
            ~f:(fun acc (clock1, clock2) ->
              if clocks_overlap clock1 clock2 then
                (* Stdio.printf "\tclocks overlap %s -> %s\n" (show_clock clock1)
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

let parse_input l =
  Sequence.unfold ~init:l ~f:(function
    | [] -> None
    | l ->
        let l = match List.hd_exn l with "" -> List.tl_exn l | _ -> l in
        let input, rest = List.split_n l 4 in
        Some (parse_lines input, rest))

let part1 l =
  Sequence.sum
    (module Int)
    (parse_input l)
    ~f:(fun sl ->
      let r = Sequence.sum (module Int) (signals sl) ~f:Fn.id in
      Stdio.printf "%d\n" r;
      r)

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

let part2 = part1

let p2_example1 =
  [
    "hands 6:8:12";
    "direction CW:CCW:CW";
    "current 2:4:8";
    "move 9:7:4";
    "";
    "hands 6:8:12";
    "direction CW:CCW:CW";
    "current 1:2:2";
    "move 10:12:15";
    "";
    "hands 6:8:12";
    "direction CW:CCW:CW";
    "current 1:2:2";
    "move 10:12:20";
  ]

let%test_unit "part2 (given)" =
  [%test_result: int] ~expect:21 (part2 p2_example1)

let%test_unit "part2 (apparently my only bug" =
  [%test_result: int] ~expect:1
    (part2
       [ "hands 10:10:10"; "direction CW:CW:CW"; "current 9:0:5"; "move 1:0:0" ])

let part3 = part1
