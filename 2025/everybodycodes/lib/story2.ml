[@@@ocaml.warning "-31-32"]

open Base
open Core
open Util

type board = Board of (int * int) * string

let parse_lines l =
  let idx = fst @@ List.findi_exn ~f:(fun _ s -> String.equal "" s) l in
  match List.split_n l idx with
  | l1, l2 ->
      ( Board
          ((String.length (List.hd_exn l1), List.length l1), String.concat l1),
        List.drop l2 1 )

let board_drop (Board ((xmax, ymax), grid)) token slot =
  let x, y, token_idx = (ref ((slot - 1) * 2), ref 0, ref 0) in
  while !y < ymax do
    (* Stdio.printf "[%d] x=%d, y=%d %c %c\n" slot !x !y *)
    (* grid.[(!y * xmax) + !x]
       token.[!token_idx]; *)
    (match grid.[(!y * xmax) + !x] with
    | '.' -> ()
    | '*' ->
        (match token.[!token_idx] with
        | 'L' -> x := if !x = 0 then 1 else !x - 1
        | 'R' -> x := if !x = xmax - 1 then xmax - 2 else !x + 1
        | _ -> failwith "invalid token");
        token_idx := !token_idx + 1
    | _ -> failwith "invalid grid entry");
    y := !y + 1
  done;
  (* Stdio.printf "[%d - final] x=%d, y=%d\n" slot !x !y; *)
  (!x / 2) + 1

let test_board =
  [
    "*.*.*.*.*";
    ".*.*.*.*.";
    "*...*.*.*";
    ".*.*...*.";
    "*.*...*..";
    ".*.*.*.*.";
    "";
  ]

let%test_unit "quest 1 part1 (given)" =
  [%test_eq: int] 4
    (let board, _ = parse_lines test_board in
     board_drop board "RLLRLR" 5)

let%test_unit "quest 1 part1 (given 2)" =
  [%test_eq: int] 4
    (let board, _ = parse_lines test_board in
     board_drop board "RLLRLR" 4)

let test_board =
  [
    "*.*.*.*.*.*.*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "*.*.*...*.*...*..";
    ".*.*.*.*.*...*.*.";
    "*.*.....*...*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "*...*...*.*.*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "*.*.*...*.*.*.*.*";
    ".*...*...*.*.*.*.";
    "*.*.*.*.*.*.*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "";
    "RRRLRLRRRRRL";
    "LLLLRLRRRRRR";
    "RLLLLLRLRLRL";
    "LRLLLRRRLRLR";
    "LLRLLRLLLRRL";
    "LRLRLLLRRRRL";
    "LRLLLLLLRLLL";
    "RRLLLRLLRLRR";
    "RLLLLLRLLLRL";
  ]

let tokens_won board token slot_num =
  max 0 ((board_drop board token slot_num * 2) - slot_num)

let quest1part1 l =
  let board, tokens = parse_lines l in
  tokens
  |> List.mapi ~f:(fun n token -> tokens_won board token (n + 1))
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest 1 quest1part1 (coins won)" =
  [%test_eq: int] 26 (quest1part1 test_board)

let quest1part2 l =
  let board, tokens = parse_lines l in
  match board with
  | Board ((xmax, _), _) ->
      tokens
      |> List.map ~f:(fun token ->
             1 -- ((xmax + 1) / 2)
             |> List.map ~f:(fun slot_num -> tokens_won board token slot_num)
             |> List.max_elt ~compare |> Option.value_exn)
      |> List.fold ~f:( + ) ~init:0

let%test_unit "quest1 quest1part2 (coins won)" =
  [%test_eq: int] 115
    (quest1part2
       [
         "*.*.*.*.*.*.*.*.*.*.*.*.*";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "..*.*.*.*...*.*...*.*.*..";
         ".*...*.*.*.*.*.*.....*.*.";
         "*.*...*.*.*.*.*.*...*.*.*";
         ".*.*.*.*.*.*.*.*.......*.";
         "*.*.*.*.*.*.*.*.*.*...*..";
         ".*.*.*.*.*.*.*.*.....*.*.";
         "*.*...*.*.*.*.*.*.*.*....";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "*.*.*.*.*.*.*.*.*.*.*.*.*";
         ".*.*.*.*.*.*.*.*.*...*.*.";
         "*.*.*.*.*.*.*.*.*...*.*.*";
         ".*.*.*.*.*.*.*.*.....*.*.";
         "*.*.*.*.*.*.*.*...*...*.*";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "*.*.*...*.*.*.*.*.*.*.*.*";
         ".*...*.*.*.*...*.*.*...*.";
         "*.*.*.*.*.*.*.*.*.*.*.*.*";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "";
         "RRRLLRRRLLRLRRLLLRLR";
         "RRRRRRRRRRLRRRRRLLRR";
         "LLLLLLLLRLRRLLRRLRLL";
         "RRRLLRRRLLRLLRLLLRRL";
         "RLRLLLRRLRRRLRRLRRRL";
         "LLLLLLLLRLLRRLLRLLLL";
         "LRLLRRLRLLLLLLLRLRRL";
         "LRLLRRLLLRRRRRLRRLRR";
         "LRLLRRLRLLRLRRLLLRLL";
         "RLLRRRRLRLRLRLRLLRRL";
       ])

(* part 3 is brute forceable *)

let num_slots (Board ((xmax, _), _)) = (xmax + 1) / 2

let build num_slots token_to_slot_mapping =
  let num_tokens = Hashtbl.length token_to_slot_mapping in
  let tokens = Array.of_list @@ Hashtbl.keys token_to_slot_mapping in
  let open Lp in
  let x =
    Array.init (Array.length tokens) ~f:(fun n ->
        Array.init num_slots ~f:(fun k ->
            Lp.Poly.var ~integer:true ~lb:0. ~ub:1.
              (Printf.sprintf "x_{%d%d}" n k)))
  in
  let obj =
    List.fold ~init:(c 0.0) ~f:( ++ )
      (List.map
         ~f:(fun (n, k) ->
           x.(n).(k)
           *~ c
                (* slots are 1-indexed *)
                (float_of_int
                   (Array.get
                      (Hashtbl.find_exn token_to_slot_mapping tokens.(n))
                      k)))
         (List.cartesian_product
            (Util.( -- ) 0 (num_tokens - 1))
            (Util.( -- ) 0 (num_slots - 1))))
  in
  let token_matching =
    List.init (Array.length tokens) ~f:(fun n ->
        eq (c 1.0)
          (List.fold ~f:( ++ ) ~init:(c 0.0)
             (List.init num_slots ~f:(fun k -> x.(n).(k)))))
  in
  let slot_matching =
    List.init num_slots ~f:(fun k ->
        eq (c 1.0)
          (List.fold ~f:( ++ ) ~init:(c 0.0)
             (List.init num_tokens ~f:(fun n -> x.(n).(k)))))
  in
  (make (maximize obj) (token_matching @ slot_matching), x)

(* copied from ocaml-lp, available in 0.5.0 *)

let compute_term pmap t =
  let open Lp in
  match t with
  | Term.Const f -> f
  | Term.Linear (f, x) -> f *. PMap.find (Poly.of_var x) pmap
  | Term.Quad (f, x, y) ->
      f *. PMap.find (Poly.of_var x) pmap *. PMap.find (Poly.of_var y) pmap

let compute_poly pmap p =
  Lp.Poly.map (compute_term pmap) p |> List.fold ~f:( +. ) ~init:0.

let solve num_slots token_to_slot_mapping =
  let problem, _ = build num_slots token_to_slot_mapping in
  Stdio.printf "%s\n" (Lp.Problem.to_string problem);
  match Lp_glpk.Milp.solve problem with
  | Ok (_, _) -> Stdio.printf "solved!\n"
  | Error msg -> Stdio.printf "Failed to solve: %s\n" msg

let quest1part3 l =
  let board, tokens = parse_lines l in
  let token_to_slot_mapping = Hashtbl.create (module String) in
  tokens
  |> List.iter ~f:(fun token ->
         Hashtbl.set token_to_slot_mapping ~key:token
           ~data:
             (Array.init (num_slots board) ~f:(fun s ->
                  tokens_won board token (s + 1))));
  solve (num_slots board) token_to_slot_mapping

(* let _ =
   quest1part3
     [
       "*.*.*.*.*.*.*.*.*";
       ".*.*.*.*.*.*.*.*.";
       "*.*.*...*.*...*..";
       ".*.*.*.*.*...*.*.";
       "*.*.....*...*.*.*";
       ".*.*.*.*.*.*.*.*.";
       "*...*...*.*.*.*.*";
       ".*.*.*.*.*.*.*.*.";
       "*.*.*...*.*.*.*.*";
       ".*...*...*.*.*.*.";
       "*.*.*.*.*.*.*.*.*";
       ".*.*.*.*.*.*.*.*.";
       "";
       "RRRLRLRRRRRL";
       "LLLLRLRRRRRR";
       "RLLLLLRLRLRL";
       "LRLLLRRRLRLR";
       "LLRLLRLLLRRL";
       "LRLRLLLRRRRL";
     ] *)
(*
let x = Lp.var "x"
let y = Lp.var "y"

let problem =
  let open Lp in
  let obj = maximize (x ++ y) in
  let c0 = x ++ (c 1.2 *~ y) <~ c 5.0 in
  let c1 = (c 2.0 *~ x) ++ y <~ c 1.2 in
  make obj [ c0; c1 ]

let write () = Lp.write "my_problem.lp" problem

let solve () =
  (* For other interfaces, use Lp_glpk_js or Lp_gurobi instead *)
  match Lp_glpk.solve problem with
  | Ok (obj, xs) ->
      Printf.printf "Objective: %.2f\n" obj;
      Printf.printf "x: %.2f y: %.2f\n" (Lp.PMap.find x xs) (Lp.PMap.find y xs)
  | Error msg -> print_endline msg

let () =
  if Lp.validate problem then (
    write ();
    solve ())
  else print_endline "Oops, my problem is broken." *)
