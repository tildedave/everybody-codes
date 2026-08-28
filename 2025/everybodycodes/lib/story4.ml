[@@@ocaml.warning "-32"]

open Base
open Util

let parse_sequence s = List.map (String.split ~on:',' s) ~f:Int.of_string

let%test_unit "parse_sequence" =
  [%test_eq: int list] (parse_sequence "1,2,3,4,5") [ 1; 2; 3; 4; 5 ]

let run_sequence l =
  l
  |> List.fold
       ~init:(0, Set.empty (module Int))
       ~f:(fun (n, seen) inc ->
         let back_next, forward_next = (n - inc, n + inc) in
         if back_next > 0 && not (Set.mem seen back_next) then
           (back_next, Set.add seen back_next)
         else (forward_next, Set.add seen forward_next))
  |> fst

let%test_unit "run_sequence (given)" =
  [%test_eq: int] (run_sequence @@ parse_sequence "1,2,3,4,5,6,7,8,9") 21

let%test_unit "run_sequence (given 2)" =
  [%test_eq: int]
    (run_sequence
    @@ parse_sequence
         "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30"
    )
    45

let quest1part1 lines =
  lines
  |> List.map ~f:(fun s -> run_sequence @@ parse_sequence s)
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest1part1 (given)" =
  [%test_eq: int]
    (quest1part1
       [
         "1,2,3,4,5,6,7,8,9";
         "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30";
       ])
    66

let%test_unit "quest1part1 (given 2)" =
  [%test_eq: int]
    (quest1part1
       [
         "1,1,1,1,1";
         "5,1,2,3,4,5,1,2,3,4";
         "2,1,1,2,1,1,2,1,1,2,1,1";
         "5,1,2,1,2,7,1,2,1,2,7,1,2,1,2";
       ])
    34

let run_sequence_part2 l =
  l
  |> List.fold
       ~init:(0, Set.empty (module Int))
       ~f:(fun (n, seen) inc ->
         let back_next = n - inc in
         if back_next > 0 && not (Set.mem seen back_next) then
           (back_next, Set.add seen back_next)
         else
           let forward_next =
             Sequence.unfold ~init:(n + inc) ~f:(fun n -> Some (n, n + 1))
             |> Sequence.drop_while ~f:(Set.mem seen)
             |> Sequence.hd_exn
           in
           (forward_next, Set.add seen forward_next))
  |> fst

let%test_unit "run_sequence_part2 (given)" =
  [%test_eq: int] (run_sequence_part2 @@ parse_sequence "1,1,1,1,1") 5

let%test_unit "run_sequence_part2 (given 2)" =
  [%test_eq: int]
    (run_sequence_part2 @@ parse_sequence "5,1,2,3,4,5,1,2,3,4")
    13

let quest1part2 lines =
  lines
  |> List.map ~f:(fun s -> run_sequence_part2 @@ parse_sequence s)
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest1part2 (given)" =
  [%test_eq: int]
    (quest1part2
       [
         "1,1,1,1,1";
         "5,1,2,3,4,5,1,2,3,4";
         "2,1,1,2,1,1,2,1,1,2,1,1";
         "5,1,2,1,2,7,1,2,1,2,7,1,2,1,2";
       ])
    43

(* part 3 asks us to care about "crossings".
    easy to detect a crossing: if going from x to y, there's a crossing if an
    existing line begins or ends in the middle.

    only difficulty seems to be the forward jump increment logic, as written it is
    infinite.  I suppose if we are ever checking above the max point and can't
    make a jump without crossing something, we can give up.

    so we keep track of backwards and forwards jumps differently, as pairs.
    let's do it
*)

let avoids_crossings (x, y) arcs =
  assert (x < y);
  arcs
  |> List.filter ~f:(fun (h, t) ->
         assert (h < t);
         if h < x && x < y && y < t then false (* does not cross *)
         else if x < h && h < t && t < y then false (* also does not cross *)
         else (h < x && x < t) || (h < y && y < t))
  |> List.is_empty

let run_sequence_part3 l =
  l
  |> List.fold
       ~init:(0, Set.empty (module Int), [], [])
       ~f:(fun (n, seen, arcs, other_arcs) inc ->
         let back_next = n - inc in
         if
           back_next > 0
           && (not (Set.mem seen back_next))
           && avoids_crossings (back_next, n) arcs
         then
           ( back_next,
             Set.add seen back_next,
             other_arcs,
             (back_next, n) :: arcs )
         else
           let max_seen = Set.fold seen ~init:(n + inc) ~f:max in
           Sequence.unfold ~init:(n + inc) ~f:(fun n ->
               (* + 100 is a kludge, I want to end the process at some point *)
               if n > max_seen + 100 then None else Some (n, n + 1))
           |> Sequence.drop_while ~f:(fun n' ->
                  Set.mem seen n' || not (avoids_crossings (n, n') arcs))
           |> Sequence.hd
           |> Option.value_map ~default:(n, seen, arcs, other_arcs)
                ~f:(fun forward_next ->
                  ( forward_next,
                    Set.add seen forward_next,
                    other_arcs,
                    (n, forward_next) :: arcs )))
  |> fun (n, _, _, _) -> n

let%test_unit "run_sequence_part3 (given)" =
  [%test_eq: int] (run_sequence_part3 @@ parse_sequence "1,1,1,1,1") 5

let%test_unit "run_sequence_part3 (given, 2)" =
  [%test_eq: int]
    (run_sequence_part3 @@ parse_sequence "5,1,2,3,4,5,1,2,3,4")
    20

let quest1part3 lines =
  lines
  |> List.map ~f:(fun s -> run_sequence_part3 @@ parse_sequence s)
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest1part3 (given)" =
  [%test_eq: int]
    (quest1part3
       [
         "1,1,1,1,1";
         "5,1,2,3,4,5,1,2,3,4";
         "2,1,1,2,1,1,2,1,1,2,1,1";
         "5,1,2,1,2,7,1,2,1,2,7,1,2,1,2";
       ])
    27

let%test_unit "quest1part3 (given, part 2)" =
  [%test_eq: int]
    (quest1part3
       [
         "5,3,1,1";
         "5,3,1,1,5,1,1,3,4,8,1,1";
         "5,3,1,1,5,1,1,3,4,8,2,1";
         "10,9,9,8,8,7,7,6,6,5,5,4,4,3,3,2,2,1";
       ])
    35

(* part 2 - parsing time *)

let parse_coords s =
  Stdlib.Scanf.sscanf
    (List.nth_exn (String.split s ~on:'=') 1)
    "[%d,%d]"
    (fun x y -> (x, y))

let parse_moves s = String.to_list @@ List.nth_exn (String.split s ~on:'=') 1

let%test_unit "parse_coords (start)" =
  [%test_eq: int * int] (5, 0) (parse_coords "START=[5,0]")

type part2problem = {
  start : int * int;
  a_beacon : int * int;
  b_beacon : int * int;
  c_beacon : int * int;
  moves : char list;
}
[@@deriving compare, sexp_of, show]

let parse_problem_part2 lines =
  {
    start = parse_coords (List.nth_exn lines 0);
    a_beacon = parse_coords (List.nth_exn lines 1);
    b_beacon = parse_coords (List.nth_exn lines 2);
    c_beacon = parse_coords (List.nth_exn lines 3);
    moves = Option.value_map ~f:parse_moves ~default:[] (List.nth lines 4);
  }

let%test_unit "parse_problem_part2 (given)" =
  [%test_eq: part2problem]
    {
      start = (5, 0);
      a_beacon = (0, 0);
      b_beacon = (10, 0);
      c_beacon = (5, 10);
      moves = [ 'A'; 'B'; 'C'; 'C'; 'B'; 'A'; 'B'; 'C'; 'A' ];
    }
    (parse_problem_part2
       [ "START=[5,0]"; "A=[0,0]"; "B=[10,0]"; "C=[5,10]"; "MOVES=ABCCBABCA" ])

let halfway_to (cx, cy) (dx, dy) = ((cx + dx) / 2, (cy + dy) / 2)

let%test_unit "halfway_to (given)" =
  [%test_eq: int * int] (2, 0) (halfway_to (5, 0) (0, 0))

let%test_unit "halfway_to (given 2)" =
  [%test_eq: int * int] (7, 3) (halfway_to (5, 7) (10, 0))

let run_launch problem =
  problem.moves
  |> List.fold
       ~init:
         ( problem.start,
           Set.add (Set.empty (module IntPair_Comparator)) problem.start )
       ~f:(fun (curr_coord, seen) next_beacon ->
         let next_coord =
           halfway_to curr_coord
           @@
           match next_beacon with
           | 'A' -> problem.a_beacon
           | 'B' -> problem.b_beacon
           | 'C' -> problem.c_beacon
           | _ -> failwith "invalid input"
         in
         (next_coord, Set.add seen next_coord))
  |> snd

let quest2part1 l = l |> parse_problem_part2 |> run_launch |> Set.length

let%test_unit "quest2part1 (given)" =
  [%test_eq: int] 8
    ([ "START=[5,0]"; "A=[0,0]"; "B=[10,0]"; "C=[5,10]"; "MOVES=ABCCBABCA" ]
    |> quest2part1)

let neighbor_set (x, y) =
  Set.of_list
    (module IntPair_Comparator)
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let firefly_count lit_squares =
  let ff_squares =
    Set.fold
      ~init:(Set.empty (module IntPair_Comparator))
      ~f:(fun acc coord -> Set.union acc (neighbor_set coord))
      lit_squares
  in
  Set.length @@ Set.diff ff_squares lit_squares

let quest2part2 l = l |> parse_problem_part2 |> run_launch |> firefly_count

let%test_unit "quest2part2 (given, 1)" =
  [%test_eq: int] 25
    ([ "START=[5,0]"; "A=[0,0]"; "B=[10,0]"; "C=[5,10]"; "MOVES=ABCCBABCA" ]
    |> quest2part2)

let%test_unit "quest2part2 (given, 2)" =
  [%test_eq: int] 46
    ([
       "START=[5,0]";
       "A=[0,0]";
       "B=[10,0]";
       "C=[5,10]";
       "MOVES=BABCAABBCABCCCBBABCCCAAACABABCBCBBCAABBABBCACCBAABCBCBBBCBBBBBCCCAACAACB";
     ]
    |> quest2part2)

(* part 3 seems to just be flood fill *)
(* loop with visited + unvisited sets *)
(* then apply fireflies on top of those illuminated squares *)

let rec run_launch_flood_fill problem unvisited_set visited_set =
  match Set.choose unvisited_set with
  | None -> visited_set
  | Some coord ->
      run_launch_flood_fill problem
        (List.fold
           ~init:(Set.remove unvisited_set coord)
           ~f:(fun unvisited_set next ->
             if Set.mem visited_set next then unvisited_set
             else Set.add unvisited_set next)
           [
             halfway_to coord problem.a_beacon;
             halfway_to coord problem.b_beacon;
             halfway_to coord problem.c_beacon;
           ])
        (Set.add visited_set coord)

let quest2part3 l =
  let problem = parse_problem_part2 l in
  run_launch_flood_fill problem
    (Set.add (Set.empty (module IntPair_Comparator)) problem.start)
    (Set.empty (module IntPair_Comparator))
  |> firefly_count

let%test_unit "quest2part3 (given, 1)" =
  [%test_eq: int] 42
    ([ "START=[5,0]"; "A=[0,0]"; "B=[10,0]"; "C=[5,10]" ] |> quest2part3)

let%test_unit "quest2part3 (given, 2)" =
  [%test_eq: int] 432
    ([ "START=[0,0]"; "A=[0,0]"; "B=[80,15]"; "C=[5,30]" ] |> quest2part3)

type part3problem = {
  width : int;
  height : int;
  horizontal_offsets : int array;
  vertical_offsets : int array;
}
[@@deriving compare, sexp_of, show]

let stitch_sequence =
  Sequence.unfold ~init:true ~f:(fun b -> Some ((if b then 1 else 0), not b))

let stitches i offsets length =
  let idx = i % Array.length offsets in
  Sequence.take
    (match offsets.(idx) with
    | 0 -> stitch_sequence
    | 1 -> Sequence.tl_eagerly_exn stitch_sequence
    | _ -> failwith "invalid offset")
    length
  |> Sequence.to_array

let%test_unit "stiches (1)" =
  [%test_eq: int array] [| 0; 1; 0; 1; 0 |] (stitches 0 [| 1; 0; 0; 1; 1 |] 5)

let%test_unit "stiches (2)" =
  [%test_eq: int array] [| 1; 0; 1; 0; 1 |] (stitches 6 [| 1; 0; 0; 1; 1 |] 5)

let vertical_stitches problem =
  Array.init (problem.width + 1) ~f:(fun n ->
      stitches n problem.vertical_offsets problem.height)

let horizontal_stitches problem =
  Array.init (problem.height + 1) ~f:(fun n ->
      stitches n problem.horizontal_offsets problem.width)

let%test_unit "horizontal stitches (1)" =
  [%test_eq: int array array]
    [|
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0 |];
      [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    |]
    (horizontal_stitches
       {
         height = 5;
         width = 10;
         horizontal_offsets = [| 1; 0; 0; 1; 1 |];
         vertical_offsets = [| 1; 1; 0; 1; 1 |];
       })

let%test_unit "vertical stitches (1)" =
  [%test_eq: int array array]
    [|
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 1; 0; 1; 0; 1; 0; 1; 0; 1; 0 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
      [| 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 |];
    |]
    (vertical_stitches
       {
         height = 10;
         width = 5;
         horizontal_offsets = [| 1; 0; 0; 1; 1 |];
         vertical_offsets = [| 1; 1; 0; 1; 1 |];
       })

let is_stitched = equal_int 1

let num_isolated problem =
  let h_stitches, v_stitches =
    (horizontal_stitches problem, vertical_stitches problem)
  in
  let answer = ref 0 in
  for y = 0 to problem.height - 1 do
    for x = 0 to problem.width - 1 do
      if
        List.for_all
          [
            h_stitches.(y).(x);
            h_stitches.(y + 1).(x);
            v_stitches.(x).(y);
            v_stitches.(x + 1).(y);
          ]
          ~f:is_stitched
      then answer := !answer + 1
    done
  done;
  !answer

let%test_unit "num_isolated" =
  [%test_eq: int] 27
    (num_isolated
       {
         height = 30;
         width = 10;
         horizontal_offsets = [| 1; 0; 0; 1; 1 |];
         vertical_offsets = [| 1; 1; 0; 1; 1 |];
       })

let parse_problem_part3 lines =
  let after_equals s = List.nth_exn (String.split s ~on:'=') 1 in
  {
    width = List.nth_exn lines 0 |> after_equals |> Int.of_string;
    height = List.nth_exn lines 1 |> after_equals |> Int.of_string;
    horizontal_offsets =
      List.nth_exn lines 2 |> after_equals |> String.to_list
      |> List.map ~f:(fun ch ->
             match ch with '0' -> 0 | '1' -> 1 | _ -> failwith "invalid")
      |> Array.of_list;
    vertical_offsets =
      List.nth_exn lines 3 |> after_equals |> String.to_list
      |> List.map ~f:(fun ch ->
             match ch with '0' -> 0 | '1' -> 1 | _ -> failwith "invalid")
      |> Array.of_list;
  }

let quest3part1 l = num_isolated @@ parse_problem_part3 l

let%test_unit "quest3part1" =
  [%test_eq: int] 27
    (quest3part1
       [
         "width=30";
         "height=10";
         "horizontal-offsets=10011";
         "vertical-offsets=11011";
       ])
