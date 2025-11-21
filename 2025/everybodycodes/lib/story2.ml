[@@@ocaml.warning "-31-32"]

open Base
open Core
open Util
open Continue_or_stop

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

let build num_slots token_to_slot_mapping ~max =
  let num_tokens = Hashtbl.length token_to_slot_mapping in
  let tokens = Array.of_list @@ Hashtbl.keys token_to_slot_mapping in
  let open Lp in
  let pairs =
    List.cartesian_product
      (Util.( -- ) 0 (num_tokens - 1))
      (Util.( -- ) 0 (num_slots - 1))
  in
  let x =
    Array.init (Array.length tokens) ~f:(fun n ->
        Array.init num_slots ~f:(fun k ->
            Lp.Poly.var ~integer:true ~lb:0. ~ub:1. (Printf.sprintf "x%d%d" n k)))
  in
  let z = Lp.var "z" in
  let z_constraint =
    eq z
      (List.fold ~init:(c 0.0) ~f:( ++ )
         (List.map
            ~f:(fun (n, k) ->
              x.(n).(k)
              *~ c
                   (float_of_int
                      (Hashtbl.find_exn token_to_slot_mapping tokens.(n)).(k)))
            pairs))
  in
  let token_matching =
    List.init (Array.length tokens) ~f:(fun n ->
        eq
          (List.fold ~f:( ++ ) ~init:(c 0.0)
             (List.init num_slots ~f:(fun k -> x.(n).(k))))
          (c 1.0))
  in
  let slot_matching =
    List.init num_slots ~f:(fun k ->
        lt
          (List.fold ~f:( ++ ) ~init:(c 0.0)
             (List.init num_tokens ~f:(fun n -> x.(n).(k))))
          (c 1.0))
  in
  ( make
      (if max then maximize z else minimize z)
      ((z_constraint :: token_matching) @ slot_matching),
    z )

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

let solve num_slots token_to_slot_mapping ~max =
  let problem, z = build num_slots token_to_slot_mapping ~max in
  match Lp_glpk.Milp.solve problem with
  | Ok (_, pmap) -> Int.of_float (compute_poly pmap z)
  | Error msg ->
      Stdio.printf "Failed to solve: %s\n" msg;
      assert false

let quest1part3 l =
  let board, tokens = parse_lines l in
  let token_to_slot_mapping = Hashtbl.create (module String) in
  tokens
  |> List.iter ~f:(fun token ->
         Hashtbl.set token_to_slot_mapping ~key:token
           ~data:
             (Array.init (num_slots board) ~f:(fun s ->
                  tokens_won board token (s + 1))));
  Printf.sprintf "%d %d"
    (solve (num_slots board) token_to_slot_mapping ~max:false)
    (solve (num_slots board) token_to_slot_mapping ~max:true)
(*
let%test_unit "quest1part3 (given, 1)" =
  [%test_eq: string] "13 43"
    (quest1part3
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
       ]) *)

let next_bolt ch =
  match ch with
  | 'R' -> 'G'
  | 'G' -> 'B'
  | 'B' -> 'R'
  | _ -> failwith "invalid balloon"

let quest2part1 s =
  let rec loop i bolt num_bolts =
    if i = String.length s then num_bolts
    else if equal_char s.[i] bolt then loop (i + 1) bolt num_bolts
    else if i + 1 = String.length s then num_bolts
    else loop (i + 1) (next_bolt bolt) (num_bolts + 1)
  in
  loop 0 'R' 1

let%test_unit "quest 2 part1 (given)" =
  [%test_eq: int] 7 (quest2part1 "GRBGGGBBBRRRRRRRR")
(* pretty sure part 2 was a advent of code problem *)
(* OK it's a modified variant of the Josephus problem *)
(* I guess we can solve this with a linked list that we modify *)

let find_middle (list, length) =
  1 -- (length / 2)
  |> List.fold
       ~init:(Option.value_exn @@ Doubly_linked.first_elt list)
       ~f:(fun acc _ -> Option.value_exn @@ Doubly_linked.next list acc)
  |> Some

let create_list s num_repeats =
  let list, length = (Doubly_linked.create (), num_repeats * String.length s) in
  for _ = 1 to num_repeats do
    for j = 0 to String.length s - 1 do
      let _ = Doubly_linked.insert_last list s.[j] in
      ()
    done
  done;
  let middle = find_middle (list, length) in
  (list, middle, length)

let%test_unit "create_list" =
  [%test_eq: string] "GGBRGGBRGGBRGGBRGGBR"
    (Doubly_linked.fold ~init:""
       ~f:(fun acc ch -> String.append acc (Char.to_string ch))
       (match create_list "GGBR" 5 with s, _, _ -> s))

let%test_unit "find_middle" =
  [%test_eq: char] 'B'
    (Doubly_linked.Elt.value
       (match create_list "GGBR" 5 with _, m, _ -> Option.value_exn m))

let shoot_fluffbolt ch (list, middle_elt, length) =
  (* Stdio.printf "[%c] (%c, %c, %d)\n" ch
     (Doubly_linked.first_exn list)
     (Doubly_linked.Elt.value middle_elt)
     length; *)
  let next_middle_elt =
    if length % 2 = 0 then Doubly_linked.next list (Option.value_exn middle_elt)
    else middle_elt
  in
  let first_elt = Option.value_exn @@ Doubly_linked.first_elt list in
  if length % 2 = 0 && equal_char (Doubly_linked.Elt.value first_elt) ch then (
    ignore (Doubly_linked.remove_first list);
    ignore (Doubly_linked.remove list (Option.value_exn middle_elt));
    (list, next_middle_elt, length - 2))
  else (
    ignore (Doubly_linked.remove_first list);
    (list, next_middle_elt, length - 1))

let shoot_in_circle num_repeats s =
  let rec loop state bolt num_bolts =
    let next_state = shoot_fluffbolt bolt state in
    match next_state with
    | _, _, 0 -> num_bolts + 1
    | _ -> loop next_state (next_bolt bolt) (num_bolts + 1)
  in
  loop (create_list s num_repeats) 'R' 0

let%test_unit "shoot_in_circle" = [%test_eq: int] 14 (shoot_in_circle 5 "GGBR")

let%test_unit "shoot_in_circle (longer; 1)" =
  [%test_eq: int] 304 (shoot_in_circle 10 "BBRGGRRGBBRGGBRGBBRRBRRRBGGRRRBGBGG")

let%test_unit "shoot_in_circle (longer; 2)" =
  [%test_eq: int] 1464
    (shoot_in_circle 50 "BBRGGRRGBBRGGBRGBBRRBRRRBGGRRRBGBGG")

let%test_unit "shoot_in_circle (longer; 3)" =
  [%test_eq: int] 2955
    (shoot_in_circle 100 "BBRGGRRGBBRGGBRGBBRRBRRRBGGRRRBGBGG")

let quest2part2 = shoot_in_circle 100
let quest2part3 = shoot_in_circle 100000

(* quest 3 - well, seems likely we are going to be solving some matrices before the end of this *)

type die = {
  id : int;
  faces : int array;
  active_face : int;
  pulse : int;
  roll_number : int;
  seed : int;
}

let roll_die { id; faces; pulse; active_face; roll_number; seed } =
  let spin = roll_number * pulse in
  let active_face = (active_face + spin) % Array.length faces in
  let result = faces.(active_face) in
  ( result,
    {
      id;
      faces;
      pulse = ((pulse + spin) % seed) + 1 + roll_number + seed;
      active_face;
      roll_number = roll_number + 1;
      seed;
    } )

let die_re = Re.Perl.re {|(\d+): faces=\[([^]]*)\] seed=(\d+)|} |> Re.compile

let parse_die s =
  match Re.exec_opt die_re s with
  | None -> failwith "could not parse"
  | Some groups ->
      let id, faces, seed =
        ( Int.of_string @@ Re.Group.get groups 1,
          Array.map ~f:Int.of_string
            (Array.of_list (String.split ~on:',' @@ Re.Group.get groups 2)),
          Int.of_string @@ Re.Group.get groups 3 )
      in
      { id; faces; active_face = 0; pulse = seed; roll_number = 1; seed }

let roll die = Sequence.unfold ~init:die ~f:(fun die -> Some (roll_die die))

let%test_unit "roll_die (given)" =
  [%test_eq: int list]
    [ -1; 9; -1; -1; 5; 4; 4; 2; 5; 2 ]
    (Sequence.to_list
       (Sequence.take (roll (parse_die "1: faces=[1,2,4,-1,5,7,9] seed=3")) 10))

let roll_many_total die_list =
  Sequence.unfold ~init:die_list ~f:(fun die_list ->
      let rolls, next_die = die_list |> List.map ~f:roll_die |> List.unzip in
      Some (List.fold ~f:( + ) ~init:0 rolls, next_die))

let quest3part1 l =
  Sequence.fold_until ~init:(0, 0)
    ~f:(fun (num_rolls, total) round_winnings ->
      if total >= 10000 then Stop num_rolls
      else Continue (num_rolls + 1, total + round_winnings))
    (roll_many_total (List.map ~f:parse_die l))
    ~finish:(fun _ -> 0)

let%test_unit "part1 (given)" =
  [%test_eq: int] 844
    (quest3part1
       [
         "1: faces=[1,2,3,4,5,6] seed=7";
         "2: faces=[-1,1,-1,1,-1] seed=13";
         "3: faces=[9,8,7,8,9] seed=17";
       ])

let roll_many die_list =
  Sequence.unfold ~init:die_list ~f:(fun die_list ->
      let rolls, next_die = die_list |> List.map ~f:roll_die |> List.unzip in
      Some (rolls, next_die))

type player_position = Active of int | Completed of int

let quest3part2 l =
  let die, board =
    ( l
      |> List.take_while ~f:(fun s -> not (String.is_empty s))
      |> List.map ~f:parse_die,
      l |> List.last_exn |> String.to_list
      |> List.map ~f:(fun ch -> Int.of_string @@ Char.to_string ch) )
  in
  let board_length = List.length board in
  let num_players = List.length die in
  let finish_rounds =
    Sequence.fold_until
      ~init:(0, List.init num_players ~f:(fun _ -> Active 0))
      ~f:(fun (round_num, player_state) die_results ->
        let next_positions =
          List.zip_exn die_results player_state
          |> List.map ~f:(fun (result, pos) ->
                 match pos with
                 | Active p ->
                     if result = List.nth_exn board p then
                       if p + 1 = board_length then Completed round_num
                       else Active (p + 1)
                     else pos
                 | Completed _ -> pos)
        in
        if
          List.is_empty
            (List.filter next_positions ~f:(fun q ->
                 match q with Active _ -> true | _ -> false))
        then Stop next_positions
        else Continue (round_num + 1, next_positions))
      ~finish:(fun _ -> []) (* will never finish *)
      (roll_many die)
  in
  finish_rounds
  |> List.mapi ~f:(fun n k ->
         match k with
         | Active _ -> failwith "impossible"
         | Completed a -> (n + 1, a))
  |> List.sort ~compare:(fun (_, b1) (_, b2) -> compare b1 b2)
  |> List.map ~f:(fun c -> Int.to_string @@ fst c)
  |> String.concat ~sep:","

let%test_unit "part2 (given)" =
  [%test_eq: string] "1,3,4,2"
    (quest3part2
       [
         "1: faces=[1,2,3,4,5,6,7,8,9] seed=13";
         "2: faces=[1,2,3,4,5,6,7,8,9] seed=29";
         "3: faces=[1,2,3,4,5,6,7,8,9] seed=37";
         "4: faces=[1,2,3,4,5,6,7,8,9] seed=43";
         "";
         "51257284";
       ])

let quest3part3 _ = 0
