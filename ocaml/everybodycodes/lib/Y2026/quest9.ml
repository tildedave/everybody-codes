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
        let new_size = growth / 2 in
        (* split *)
        Branch (Segment new_size, Segment 1, Segment new_size)
      else Segment (n + 1)
  | Branch (t1, t2, t3) ->
      Branch (grow ~growth t1, grow ~growth t2, grow ~growth t3)

let rec total_size plant =
  match plant with
  | Segment n -> n
  | Branch (t1, t2, t3) -> total_size t1 + total_size t2 + total_size t3 + 1

(* OK so we overcount total_leaves since each branch has a "down" segment *)

let rec total_leaves plant =
  match plant with
  | Segment _ -> 1
  | Branch (t1, t2, t3) ->
      List.fold ~init:0 ~f:( + )
        [
          (match t1 with Segment _ -> 1 | _ -> total_leaves t1 - 1);
          (match t2 with Segment _ -> 1 | _ -> total_leaves t2 - 1);
          (match t3 with Segment _ -> 1 | _ -> total_leaves t3 - 1);
        ]

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

let part2 =
  List.sum
    (module Int)
    ~f:(fun s ->
      let growth, limit = parse_line s in
      stopping_time (Sequence.map ~f:total_leaves (grow_seq ~growth)) ~limit)

(* I don't understand how this is wrong but my answer is right *)
let%test_unit "total leaves seq" =
  [%test_eq: int list]
    [ 1; 1; 1; 2; 2; 5; 5; 14 ]
    (Sequence.to_list
       (Sequence.take (Sequence.map ~f:total_leaves (grow_seq ~growth:3)) 7))

let%test_unit "part2" = [%test_eq: int] 42 (part2 example)

(* OK as expected part3 needs a closed form solution *)

let part3 _ = 1
