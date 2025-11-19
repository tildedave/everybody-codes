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
