open Base
open Core
open Util

type operation = Add | Sub
type expr = Op of operation * int * int

let parse_operation ch =
  match ch with '+' -> Add | '-' -> Sub | _ -> invalid_arg "bad operation"

let parse_line s =
  Stdlib.Scanf.sscanf s "%d %c %d = %d %c %d" (fun a o b c o' d ->
      (Op (parse_operation o, a, b), Op (parse_operation o', c, d)))

let eval (Op (op, a, b)) = match op with Add -> a + b | Sub -> a - b

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

let segments_of_operation op = match op with Add -> 2 | Sub -> 1

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

type digit = A | Num of int [@@deriving sexp_of, equal, compare]
type expr_p2 = Op2 of operation * digit list * digit list

let num_of_digit x = match x with A -> 10 | Num d -> d

let num_value l =
  List.fold ~f:(fun acc x -> (acc * 11) + num_of_digit x) l ~init:0

let%test_unit "num_value" =
  [%test_result: int] ~expect:((2 * 11) + 10) (num_value [ Num 2; A ])

let%test_unit "num_value (multiple)" =
  [%test_result: int]
    ~expect:((4 * (11 * 11 * 11)) + (8 * (11 * 11)) + (9 * 11) + 10)
    (num_value [ Num 4; Num 8; Num 9; A ])

let digit_re = Re.Perl.re {|(\d|A)+|} |> Re.compile

let parse_digit s =
  match Re.exec_opt digit_re s with
  | Some groups ->
      List.map
        ~f:(function 'A' -> A | ch -> Num (char_to_int ch))
        (String.to_list (Re.Group.get groups 0))
  | None -> invalid_arg "invalid base11 digit"

let%test_unit "parse_digit" =
  [%test_result: digit list] ~expect:[ Num 2; A ] (parse_digit "2A")

let%test_unit "parse_digit (single)" =
  [%test_result: digit list] ~expect:[ Num 9 ] (parse_digit "9")

let%test_unit "parse_digit (multiple)" =
  [%test_result: digit list] ~expect:[ Num 4; Num 8; Num 9; A ]
    (parse_digit "489A")

let segments_of_digits x = match x with Num x -> segments_of_digits x | A -> 6

let segments_of_expr_p2 (Op2 (op, a, b)) =
  List.sum (module Int) ~f:segments_of_digits a
  + List.sum (module Int) ~f:segments_of_digits b
  + segments_of_operation op

let eval_p2 (Op2 (op, a, b)) =
  match op with
  | Add -> num_value a + num_value b
  | Sub -> num_value a - num_value b

let is_valid_p2 expr1 expr2 = equal_int (eval_p2 expr1) (eval_p2 expr2)

let parse_p2 s =
  Stdlib.Scanf.sscanf s "%s %c %s = %s %c %s" (fun a o b c o' d ->
      ( Op2 (parse_operation o, parse_digit a, parse_digit b),
        Op2 (parse_operation o', parse_digit c, parse_digit d) ))

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

(* OK, for part3 we have two types of transitions: one where a digit is
   transformed by changing a segment inside it, one where a digit "lends" a
   segment and another "takes" it (including - going to +) *)

(* these are where you receive a digit *)
let digit_donee_transitions d =
  Sequence.of_list
  @@
  match d with
  | Num 0 | Num 6 | Num 9 | A -> [ Num 8 ]
  | Num 1 -> [ Num 7 ]
  | Num 3 -> [ Num 9 ]
  | Num 5 -> [ Num 6; Num 9 ]
  | Num 2 | Num 4 | Num 7 | Num 8 -> []
  | Num _ -> failwith "invalid"

(* we give away a segment *)
(* could compute this from the donee transitions actually *)
let digit_donor_transitions d =
  Sequence.of_list
  @@
  match d with
  | Num 0 | Num 1 | Num 2 | Num 3 | Num 4 | Num 5 -> []
  | Num 6 -> [ Num 5 ]
  | Num 7 -> [ Num 1 ]
  | Num 8 -> [ A; Num 6; Num 0; Num 9 ]
  | Num 9 -> [ Num 3; Num 5 ]
  | Num _ -> failwith "invalid"
  | A -> []

let digit_no_loan_transitions d =
  Sequence.of_list
  @@
  match d with
  | Num 0 -> [ Num 9; Num 6; A ]
  | Num 2 -> [ Num 3 ]
  | Num 3 -> [ Num 2; Num 5 ]
  | Num 5 -> [ Num 3 ]
  | Num 6 -> [ Num 0; Num 9; A ]
  | Num 9 -> [ Num 0; Num 6; A ]
  | Num 1 | Num 4 | Num 7 | Num 8 -> []
  | Num _ -> failwith "invalid"
  | A -> [ Num 6; Num 9; Num 0 ]

let all_digits =
  [ Num 0; Num 1; Num 2; Num 3; Num 4; Num 5; Num 6; Num 7; Num 8; Num 9; A ]

let%test_unit "donee transitions reduces digits" =
  [%test_result: unit] ~expect:()
    (List.iter
       ~f:(fun d ->
         let segments = segments_of_digits d in
         Sequence.iter
           ~f:(fun d' ->
             assert (equal_int (segments_of_digits d') (segments + 1)))
           (digit_donee_transitions d))
       all_digits)

let%test_unit "donee transitions increases digits" =
  [%test_result: unit] ~expect:()
    (List.iter
       ~f:(fun d ->
         let segments = segments_of_digits d in
         Sequence.iter
           ~f:(fun d' ->
             assert (equal_int (segments_of_digits d') (segments - 1)))
           (digit_donor_transitions d))
       all_digits)

let%test_unit "no loan transitions maintains digits" =
  [%test_result: unit] ~expect:()
    (List.iter
       ~f:(fun d ->
         Sequence.iter
           ~f:(fun d' ->
             assert (equal_int (segments_of_digits d') (segments_of_digits d)))
           (digit_no_loan_transitions d))
       all_digits)

let rec transitions t dl =
  match dl with
  | [] -> Sequence.empty
  | d :: [] -> Sequence.map ~f:(fun d -> [ d ]) (t d)
  | d :: ds ->
      Sequence.append
        (Sequence.map ~f:(fun d -> d :: ds) (t d))
        (Sequence.map ~f:(fun ds -> d :: ds) (transitions t ds))

let digitlist_no_loan_transitions = transitions digit_no_loan_transitions
let digitlist_donor_transitions = transitions digit_donor_transitions
let digitlist_donee_transitions = transitions digit_donee_transitions

let%test_unit "digitlist_no_loan_transitions" =
  [%test_result: digit list list]
    ~expect:
      [
        [ Num 9; Num 1; A ];
        [ Num 6; Num 1; A ];
        [ A; Num 1; A ];
        [ Num 0; Num 1; Num 6 ];
        [ Num 0; Num 1; Num 9 ];
        [ Num 0; Num 1; Num 0 ];
      ]
    (Sequence.to_list (digitlist_no_loan_transitions [ Num 0; Num 1; A ]))

let%test_unit "digitlist_no_loan_transitions" =
  [%test_result: digit list list] ~expect:[]
    (Sequence.to_list (digitlist_no_loan_transitions [ Num 4; Num 1; Num 7 ]))

let expr_no_loan_transitions (Op2 (o1, dl1, dl2), Op2 (o2, dl3, dl4)) =
  Sequence.concat
  @@ Sequence.of_list
       [
         (Sequence.map ~f:(fun dl1' ->
              (Op2 (o1, dl1', dl2), Op2 (o2, dl3, dl4))))
           (digitlist_no_loan_transitions dl1);
         (Sequence.map ~f:(fun dl2' ->
              (Op2 (o1, dl1, dl2'), Op2 (o2, dl3, dl4))))
           (digitlist_no_loan_transitions dl2);
         (Sequence.map ~f:(fun dl3' ->
              (Op2 (o1, dl1, dl2), Op2 (o2, dl3', dl4))))
           (digitlist_no_loan_transitions dl3);
         (Sequence.map ~f:(fun dl4' ->
              (Op2 (o1, dl1, dl2), Op2 (o2, dl3, dl4'))))
           (digitlist_no_loan_transitions dl4);
       ]

(* we receive something *)
let expr_donee_transitions (Op2 (o1, dl1, dl2), Op2 (o2, dl3, dl4)) =
  Sequence.concat
    (Sequence.of_list
       [
         (match o1 with
         | Add -> Sequence.empty
         | Sub -> Sequence.singleton (Op2 (Add, dl1, dl2), Op2 (o2, dl3, dl4)));
         (match o2 with
         | Add -> Sequence.empty
         | Sub -> Sequence.singleton (Op2 (o1, dl1, dl2), Op2 (Add, dl3, dl4)));
         (Sequence.map ~f:(fun dl1' ->
              (Op2 (o1, dl1', dl2), Op2 (o2, dl3, dl4))))
           (digitlist_donee_transitions dl1);
         (Sequence.map ~f:(fun dl2' ->
              (Op2 (o1, dl1, dl2'), Op2 (o2, dl3, dl4))))
           (digitlist_donee_transitions dl2);
         (Sequence.map ~f:(fun dl3' ->
              (Op2 (o1, dl1, dl2), Op2 (o2, dl3', dl4))))
           (digitlist_donee_transitions dl3);
         (Sequence.map ~f:(fun dl4' ->
              (Op2 (o1, dl1, dl2), Op2 (o2, dl3, dl4'))))
           (digitlist_donee_transitions dl4);
       ])

let expr_donor_transitions (Op2 (o1, dl1, dl2), Op2 (o2, dl3, dl4)) =
  Sequence.concat
    (Sequence.of_list
       [
         (match o1 with
         | Add -> Sequence.singleton (Op2 (Sub, dl1, dl2), Op2 (o2, dl3, dl4))
         | Sub -> Sequence.empty);
         (match o2 with
         | Add -> Sequence.singleton (Op2 (o1, dl1, dl2), Op2 (Sub, dl3, dl4))
         | Sub -> Sequence.empty);
         (Sequence.map ~f:(fun dl1' ->
              (Op2 (o1, dl1', dl2), Op2 (o2, dl3, dl4))))
           (digitlist_donor_transitions dl1);
         (Sequence.map ~f:(fun dl2' ->
              (Op2 (o1, dl1, dl2'), Op2 (o2, dl3, dl4))))
           (digitlist_donor_transitions dl2);
         (Sequence.map ~f:(fun dl3' ->
              (Op2 (o1, dl1, dl2), Op2 (o2, dl3', dl4))))
           (digitlist_donor_transitions dl3);
         (Sequence.map ~f:(fun dl4' ->
              (Op2 (o1, dl1, dl2), Op2 (o2, dl3, dl4'))))
           (digitlist_donor_transitions dl4);
       ])

let expr_transitions expr_pair =
  Sequence.append
    (expr_no_loan_transitions expr_pair)
    (Sequence.concat_map ~f:expr_donee_transitions
       (expr_donor_transitions expr_pair))

let can_be_made_valid expr1 expr2 =
  Option.is_some
  @@ Sequence.find
       ~f:(Tuple2.uncurry is_valid_p2)
       (expr_transitions (expr1, expr2))

let p3_example =
  [
    "1 + 1 = 5 - 1";
    "1 + 1 = 3 - 3";
    "0 - 1 = 1 + 7";
    "1 + 1 = 1 - 6";
    "1 + 1 = 2 - 1";
    "1 + 1 = 5 + 7";
    "1 + 4 = 9 - 9";
    "1 + 1 = 2 + 9";
    "1 + 1 = 1 + 2";
  ]

let part3 l =
  l
  |> List.filter_map ~f:(fun s ->
      let result = parse_p2 s in
      if (Tuple2.uncurry is_valid_p2) result then Some result
      else if (Tuple2.uncurry can_be_made_valid) result then Some result
      else None)
  |> List.sum
       (module Int)
       ~f:(fun (e1, e2) -> segments_of_expr_p2 e1 + 2 + segments_of_expr_p2 e2)
  |> to_base11

let p3_example2 =
  [
    "4469 - 5934 = 1631 - 2AA7";
    "8471 - 2276 = 4510 + 2736";
    "A767 - 6104 = 1825 + 2538";
    "9961 + 1140 = 5963 + 2974";
    "9188 - 406A = 3598 + 1533";
    "96A6 - 1606 = 4676 - 3524";
    "9114 - 15A2 = 5258 + 2371";
    "7023 - 2209 = 1433 + 3436";
    "921A - 2696 = 2A91 + 3688";
    "8492 - 2055 = 4186 + 3260";
    "1234 + 4567 = 8901 + 2345";
  ]

let%test_unit "part3 (given)" =
  [%test_result: string] ~expect:"125" (part3 p3_example)

let%test_unit "part3 (given)" =
  [%test_result: string] ~expect:"693" (part3 p3_example2)
