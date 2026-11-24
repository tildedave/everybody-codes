open Base
open Core
open Util

type operation = Add | Sub | Multiply
type expr = Op of operation * int * int

let parse_operation ch =
  match ch with
  | '+' -> Add
  | '-' -> Sub
  | '*' -> Multiply
  | _ -> invalid_arg "bad operation"

let parse_line s =
  Stdlib.Scanf.sscanf s "%d %c %d = %d %c %d" (fun a o b c o' d ->
      (Op (parse_operation o, a, b), Op (parse_operation o', c, d)))

let eval (Op (op, a, b)) =
  match op with Add -> a + b | Multiply -> a * b | Sub -> a - b

let segments_of_digits n =
  match n with
  | 0 -> 6
  | 1 -> 2
  | 2 -> 5
  | 3 -> 5
  | 4 -> 4
  | 5 -> 5
  | 6 -> 6
  | 7 -> 3
  | 8 -> 7
  | 9 -> 6
  | _ -> invalid_arg "invalid digit"

let segments_of_operation op =
  match op with Add -> 2 | Sub -> 1 | Multiply -> 2

let segments_of_expr (Op (op, a, b)) =
  segments_of_digits a + segments_of_digits b + segments_of_operation op

let is_valid expr1 expr2 = equal_int (eval expr1) (eval expr2)

let part1 l =
  l
  |> List.filter_map ~f:(fun s ->
      let result = parse_line s in
      if (Tuple2.uncurry is_valid) result then Some result else None)
  |> List.sum
       (module Int)
       ~f:(fun (e1, e2) -> segments_of_expr e1 + 2 + segments_of_expr e2)

let example =
  [ "5 - 4 = 9 - 8"; "2 + 1 = 3 + 0"; "6 + 7 = 4 + 9"; "1 + 2 = 3 + 4" ]

let%test_unit "part1 (given)" = [%test_result: int] ~expect:75 (part1 example)

type base11digit = A | Num of int [@@deriving sexp_of, equal, compare]
type expr_p2 = Op2 of operation * base11digit list * base11digit list

let num_of_digit x = match x with A -> 10 | Num d -> d

let num_value l =
  List.fold ~f:(fun acc x -> (acc * 11) + num_of_digit x) l ~init:0

let%test_unit "num_value" =
  [%test_result: int] ~expect:((2 * 11) + 10) (num_value [ Num 2; A ])

let%test_unit "num_value (multiple)" =
  [%test_result: int]
    ~expect:((4 * (11 * 11 * 11)) + (8 * (11 * 11)) + (9 * 11) + 10)
    (num_value [ Num 4; Num 8; Num 9; A ])

let base11digit_re = Re.Perl.re {|(\d|A)+|} |> Re.compile

let parse_base11digit s =
  match Re.exec_opt base11digit_re s with
  | Some groups ->
      List.map
        ~f:(function 'A' -> A | ch -> Num (char_to_int ch))
        (String.to_list (Re.Group.get groups 0))
  | None -> invalid_arg "invalid base11 digit"

let%test_unit "parse_base11digit" =
  [%test_result: base11digit list] ~expect:[ Num 2; A ] (parse_base11digit "2A")

let%test_unit "parse_base11digit (single)" =
  [%test_result: base11digit list] ~expect:[ Num 9 ] (parse_base11digit "9")

let%test_unit "parse_base11digit (multiple)" =
  [%test_result: base11digit list] ~expect:[ Num 4; Num 8; Num 9; A ]
    (parse_base11digit "489A")

let segments_of_base11digits x =
  match x with Num x -> segments_of_digits x | A -> 6

let segments_of_expr_p2 (Op2 (op, a, b)) =
  List.sum (module Int) ~f:segments_of_base11digits a
  + List.sum (module Int) ~f:segments_of_base11digits b
  + segments_of_operation op

let eval_p2 (Op2 (op, a, b)) =
  match op with
  | Add -> num_value a + num_value b
  | Sub -> num_value a - num_value b
  | Multiply -> num_value a * num_value b

let is_valid_p2 expr1 expr2 = equal_int (eval_p2 expr1) (eval_p2 expr2)

let parse_p2 s =
  Stdlib.Scanf.sscanf s "%s %c %s = %s %c %s" (fun a o b c o' d ->
      ( Op2 (parse_operation o, parse_base11digit a, parse_base11digit b),
        Op2 (parse_operation o', parse_base11digit c, parse_base11digit d) ))

let digit_to_base11 d = if equal_int d 10 then "A" else Int.to_string d

let rec to_base11 n =
  if n < 11 then digit_to_base11 n
  else String.append (to_base11 (n / 11)) (digit_to_base11 (n % 11))

let%test_unit "to_base11" = [%test_result: string] ~expect:"101" (to_base11 122)

let part2 l =
  l
  |> List.filter_map ~f:(fun s ->
      let result = parse_p2 s in
      if (Tuple2.uncurry is_valid_p2) result then Some result else None)
  |> List.sum
       (module Int)
       ~f:(fun (e1, e2) -> segments_of_expr_p2 e1 + 2 + segments_of_expr_p2 e2)
  |> to_base11

let p2_example =
  [
    "10 + 2A = 13 + 27";
    "98 - 54 = 76 - 32";
    "10 + 12 = 30 - 9";
    "20 - 10 = 5 + 5";
  ]

let%test_unit "part2 (given)" =
  [%test_result: string] ~expect:"101" (part2 p2_example)

let part3 _ = 1
