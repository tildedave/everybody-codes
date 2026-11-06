open Base

(* OK well we'll just simulate for part1 but I assume there's a closed form *)

(* every growth steps, add n to total and split - *)
(* At the same moment, a new branch, already one element long, grew from that branching point *)
(* OK so the example of 5 shows us all the segments aren't the same *)

type plant = Segment of int | Branch of plant * plant * plant

let rec grow ~growth plant =
  match plant with
  | Segment n ->
      if equal (n + 1) growth then
        let new_size = (growth - 1) / 2 in
        (* split *)
        Branch (Segment new_size, Segment 1, Segment new_size)
      else Segment (n + 1)
  | Branch (t1, t2, t3) ->
      Branch (grow ~growth t1, grow ~growth t2, grow ~growth t3)

let rec total_size plant =
  match plant with
  | Segment n -> n
  | Branch (t1, t2, t3) -> total_size t1 + total_size t2 + total_size t3 + 1

(* based on this we can see: *)
(* total answer is 2 * # of segments - 3 * # of splits - 1 *)
(* 27 segments * 2 - 3 * 13 - 1 = 14 ---> correct *)

let _total_leaves plant =
  let rec walk plant =
    match plant with
    | Segment _ -> 2
    | Branch (t1, t2, t3) ->
        List.fold ~init:0 ~f:( + ) [ walk t1 - 1; walk t2 - 1; walk t3 - 1 ]
  in
  walk plant - 1

let grow2 ~growth (segments, num_splits) =
  Map.fold segments
    ~init:(Map.empty (module Int), num_splits)
    ~f:(fun ~key:size ~data:count (next_segments, num_splits) ->
      if equal (size + 1) growth then
        let next =
          Map.update
            (Map.update next_segments 1
               ~f:(Option.value_map ~default:count ~f:(( + ) count)))
            ((growth - 1) / 2)
            ~f:(Option.value_map ~default:(2 * count) ~f:(( + ) (2 * count)))
        in
        (next, num_splits + count)
      else
        ( Map.update next_segments (size + 1)
            ~f:(Option.value_map ~default:count ~f:(( + ) count)),
          num_splits ))

let total_leaves' (segments, num_splits) =
  (2 * List.sum (module Int) ~f:Fn.id (Map.data segments))
  - (3 * num_splits) - 1

let grow_seq ~growth =
  Sequence.unfold ~init:(Segment 0) ~f:(fun p -> Some (p, grow ~growth p))

let stopping_time seq ~limit =
  seq |> Sequence.take_while ~f:(fun n -> n < limit) |> Sequence.length

let%test_unit "stopping_time example" =
  [%test_eq: int list] [ 5; 9; 13 ]
    (List.map
       ~f:(fun (growth, limit) ->
         stopping_time (Sequence.map ~f:total_size (grow_seq ~growth)) ~limit)
       [ (3, 10); (5, 20); (7, 30) ])

let parse_line s =
  Stdlib.Scanf.sscanf s "id=%d, growth=%d, limit=%d" (fun _ growth limit ->
      (growth, limit))

let part1 =
  List.sum
    (module Int)
    ~f:(fun s ->
      let growth, limit = parse_line s in
      stopping_time (Sequence.map ~f:total_size (grow_seq ~growth)) ~limit)

let example =
  [
    "id=1, growth=3, limit=10";
    "id=2, growth=5, limit=20";
    "id=3, growth=7, limit=30";
  ]

let%test_unit "part1" = [%test_eq: int] 27 (part1 example)

let grow2_seq ~growth =
  Sequence.unfold
    ~f:(fun trees -> Some (trees, grow2 ~growth trees))
    ~init:(Map.singleton (module Int) 1 1, 0)

let part2 =
  List.sum
    (module Int)
    ~f:(fun s ->
      let growth, limit = parse_line s in
      stopping_time (Sequence.map ~f:total_leaves' (grow2_seq ~growth)) ~limit
      + 1)

let%test_unit "part2" = [%test_eq: int] 42 (part2 example)

(* OK as expected, must be smarter for part 3 *)
(* idea will be to skip all the parts where we're not splitting *)

let _print_map m =
  List.iter
    ~f:(fun (s, c) -> Stdio.printf "size %d has %d count\n" s c)
    (Map.to_alist m)

(* how many segments?  we know that through simulation *)
(* how many splits?  we will need to keep track of that since splits are not uniform *)

let part3 _ = 1
