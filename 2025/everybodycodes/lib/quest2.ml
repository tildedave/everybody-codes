open Base

let complex_add p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  (x1 + x2, y1 + y2)

let%test_unit "complex_add_1" =
  [%test_eq: int * int] (3, 3) (complex_add (1, 1) (2, 2))

let%test_unit "complex_add_2" =
  [%test_eq: int * int] (5, 12) (complex_add (2, 5) (3, 7))

let%test_unit "complex_add_3" =
  [%test_eq: int * int] (8, 4) (complex_add (-2, 5) (10, -1))

let%test_unit "complex_add_4" =
  [%test_eq: int * int] (-4, -6) (complex_add (-1, -2) (-3, -4))

let complex_mult p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  ((x1 * x2) - (y1 * y2), (x1 * y2) + (y1 * x2))

let%test_unit "complex_mult_1" =
  [%test_eq: int * int] (0, 4) (complex_mult (1, 1) (2, 2))

let%test_unit "complex_mult_2" =
  [%test_eq: int * int] (-29, 29) (complex_mult (2, 5) (3, 7))

let%test_unit "complex_mult_3" =
  [%test_eq: int * int] (-15, 52) (complex_mult (-2, 5) (10, -1))

let%test_unit "complex_mult_4" =
  [%test_eq: int * int] (-5, 10) (complex_mult (-1, -2) (-3, -4))

let complex_div p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  (x1 / x2, y1 / y2)

let%test_unit "complex_div_1" =
  [%test_eq: int * int] (5, 6) (complex_div (10, 12) (2, 2))

let%test_unit "complex_div_2" =
  [%test_eq: int * int] (3, 2) (complex_div (11, 12) (3, 5))

let%test_unit "complex_div_3" =
  [%test_eq: int * int] (-5, -6) (complex_div (-10, -12) (2, 2))

let%test_unit "complex_div_4" =
  [%test_eq: int * int] (-3, -2) (complex_div (-11, -12) (3, 5))

let parse_number s = Caml.Scanf.sscanf s "A=[%d, %d]" (fun a b -> (a, b))

let%test_unit "parse_number" =
  [%test_eq: int * int] (25, 9) (parse_number "A=[25,9]")

let part1 l =
  let a = parse_number (List.hd_exn l) in
  let x, y =
    List.fold_left (List.range 0 3) ~init:(0, 0) ~f:(fun result _ ->
        complex_add a (complex_div (complex_mult result result) (10, 10)))
  in
  Printf.sprintf "[%d,%d]" x y

let%test_unit "part 1" = [%test_eq: string] "[357,862]" (part1 [ "A=[25,9]" ])

let should_engrave p =
  let quit_loop, i, result = (ref false, ref 0, ref (0, 0)) in
  while (not !quit_loop) && !i < 100 do
    result := complex_mult !result !result;
    result := complex_div !result (100000, 100000);
    result := complex_add !result p;
    let x, y = !result in
    if x > 1000000 || y > 1000000 || x < -1000000 || y < -1000000 then
      quit_loop := true;
    i := !i + 1
  done;
  if !quit_loop then (false, !result, !i) else (true, !result, !i)

let%test_unit "should_engrave 1 (true)" =
  [%test_eq: bool * (int * int) * int]
    (true, (-2520, -5355), 100)
    (should_engrave (35630, -64880))

let%test_unit "should_engrave 2 (true)" =
  [%test_eq: bool * (int * int) * int]
    (true, (5021, 6454), 100)
    (should_engrave (35630, -64870))

let%test_unit "should_engrave 3 (true)" =
  [%test_eq: bool * (int * int) * int]
    (true, (-3291, -684), 100)
    (should_engrave (35640, -64860))

let%test_unit "should_engrave 4 (true)" =
  [%test_eq: bool * (int * int) * int]
    (true, (-7266, 3234), 100)
    (should_engrave (36230, -64270))

let%test_unit "should_engrave 5 (true)" =
  [%test_eq: bool * (int * int) * int]
    (true, (162903, -679762), 100)
    (should_engrave (36250, -64270))

let%test_unit "should_engrave 1 (false)" =
  [%test_eq: bool * (int * int) * int]
    (false, (1265017, 932533), 27)
    (should_engrave (35460, -64910))

let%test_unit "should_engrave 2 (false)" =
  [%test_eq: bool * (int * int) * int]
    (false, (-1724836, 19302), 28)
    (should_engrave (35470, -64910))

let%test_unit "should_engrave 3 (false)" =
  [%test_eq: bool * (int * int) * int]
    (false, (-575306, 8705296), 30)
    (should_engrave (35480, -64910))

let%test_unit "should_engrave 4 (false)" =
  [%test_eq: bool * (int * int) * int]
    (false, (-7919169, 5303832), 95)
    (should_engrave (35680, -64850))

let%test_unit "should_engrave 5 (false)" =
  [%test_eq: bool * (int * int) * int]
    (false, (-6387697, -1621945), 100)
    (should_engrave (35630, -64830))

(*
  let a = parse_number (List.hd_exn l) in
  let x, y =
    List.fold_left (List.range 0 3) ~init:(0, 0) ~f:(fun result _ ->
        complex_add a (complex_div (complex_mult result result) (10, 10)))
  in
  Printf.sprintf "[%d,%d]" x y *)

(* mandelbrot set lol *)
(*
let part2 l =
  let (x_start, y_start) = parse_number (List.hd_exn l) in
  let (x_end, y_end) = (x_start + 100, y_start + 100) in *)
