open Base

type spine = Empty | Spine of int option * int * int option * spine
[@@deriving sexp]

let rec insert_fishbone s x =
  match s with
  | Empty -> Spine (None, x, None, Empty)
  | Spine (None, y, None, s') ->
      if x < y then Spine (Some x, y, None, s')
      else if x > y then Spine (None, y, Some x, s')
      else Spine (None, y, None, insert_fishbone s' x)
  | Spine (l, y, None, s') ->
      if x > y then Spine (l, y, Some x, s')
      else Spine (l, y, None, insert_fishbone s' x)
  | Spine (None, y, r, s') ->
      if x < y then Spine (Some x, y, r, s')
      else Spine (None, y, r, insert_fishbone s' x)
  | Spine (l, y, r, s') -> Spine (l, y, r, insert_fishbone s' x)

let rec quality_str s =
  match s with
  | Empty -> ""
  | Spine (_, n, _, s') ->
      String.concat [ Printf.sprintf "%d" n; quality_str s' ]

let quality s = Int.of_string @@ quality_str s
let construct_fishbone l = List.fold l ~init:Empty ~f:insert_fishbone

let%test_unit "quality" =
  [%test_eq: int] 581078
    (quality @@ construct_fishbone [ 5; 3; 7; 8; 9; 10; 4; 5; 7; 8; 8 ])

let parse_sword s =
  match String.split s ~on:':' with
  | [ id; s ] ->
      (Int.of_string id, List.map ~f:Int.of_string (String.split ~on:',' s))
  | _ -> failwith "invalid input"

let part1 s =
  let _, l = parse_sword s in
  quality @@ construct_fishbone l

let%test_unit "part1 (given)" =
  [%test_eq: int] 581078 (part1 "58:5,3,7,8,9,10,4,5,7,8,8")

let part2 sl =
  let sorted = List.sort (List.map ~f:part1 sl) ~compare:compare_int in
  Printf.sprintf "%d" @@ (List.last_exn sorted - List.hd_exn sorted)

let%test_unit "part2 (given)" =
  [%test_eq: string] "77053"
    (part2
       [
         "1:2,4,1,1,8,2,7,9,8,6";
         "2:7,9,9,3,8,3,8,8,6,8";
         "3:4,7,6,9,1,8,3,7,2,2";
         "4:6,4,2,1,7,4,5,5,5,8";
         "5:2,9,3,8,3,9,5,2,1,4";
         "6:2,4,9,6,7,4,1,7,6,8";
         "7:2,3,7,6,2,2,4,1,4,2";
         "8:5,1,5,6,8,3,1,8,3,9";
         "9:5,7,7,3,7,2,3,8,6,7";
         "10:4,1,9,3,8,5,4,3,5,5";
       ])

let fishbone_level s =
  Int.of_string
  @@
  match s with
  | Spine (Some l, y, Some r, _) -> Printf.sprintf "%d%d%d" l y r
  | Spine (None, y, Some r, _) -> Printf.sprintf "%d%d" y r
  | Spine (Some l, y, None, _) -> Printf.sprintf "%d%d" l y
  | Spine (None, y, None, _) -> Printf.sprintf "%d" y
  | Empty -> failwith "never should have been called"

let rec compare_fishbone s1 s2 =
  match (s1, s2) with
  | Spine (_, _, _, s1'), Spine (_, _, _, s2') ->
      let fb1, fb2 = (fishbone_level s1, fishbone_level s2) in
      let c = compare fb1 fb2 in
      if equal_int c 0 then compare_fishbone s1' s2' else c
  | Empty, Empty -> 0
  | _ -> failwith "invalid comparison - fishbones would not have same quality"

let compare_sword s1 s2 =
  match (parse_sword s1, parse_sword s2) with
  | (s1_id, s1_l), (s2_id, s2_l) ->
      let fb1, fb2 = (construct_fishbone s1_l, construct_fishbone s2_l) in
      let qc = compare_int (quality fb1) (quality fb2) in
      if not (equal_int qc 0) then qc
      else
        let fbc = compare_fishbone fb1 fb2 in
        if not (equal_int fbc 0) then fbc else compare s1_id s2_id

let%test_unit "part3 comparator (given; single sword)" =
  [%test_eq: int] 1
    (compare_sword "1:5,3,7,8,1,10,9,5,7,8" "2:5,3,7,8,1,10,9,4,7,9")

let%test_unit "part3 comparator (given; every sword)" =
  [%test_eq: string list]
    [
      "3:7,1,9,1,6,9,8,3,8,3";
      "5:7,1,9,1,6,9,8,3,7,3";
      "6:6,1,9,2,8,8,8,4,3,5";
      "1:7,1,9,1,6,9,8,3,7,2";
      "2:6,1,9,2,9,8,8,4,3,1";
      "4:6,1,9,2,8,8,8,4,3,1";
      "9:3,7,2,2,7,4,1,6,3,7";
      "7:3,7,2,2,7,4,4,6,3,1";
      "8:3,7,2,2,7,4,4,6,3,7";
    ]
    (List.sort
       [
         "1:7,1,9,1,6,9,8,3,7,2";
         "2:6,1,9,2,9,8,8,4,3,1";
         "3:7,1,9,1,6,9,8,3,8,3";
         "4:6,1,9,2,8,8,8,4,3,1";
         "5:7,1,9,1,6,9,8,3,7,3";
         "6:6,1,9,2,8,8,8,4,3,5";
         "7:3,7,2,2,7,4,4,6,3,1";
         "8:3,7,2,2,7,4,4,6,3,7";
         "9:3,7,2,2,7,4,1,6,3,7";
       ] ~compare:(fun s1 s2 -> compare_sword s2 s1))

let part3 sl =
  snd
  @@ List.fold
       (List.sort sl ~compare:(fun s1 s2 -> compare_sword s2 s1))
       ~init:(1, 0)
       ~f:(fun (n, total) s ->
         let id, _ = parse_sword s in
         (n + 1, (n * id) + total))

let%test_unit "part3 (given)" =
  [%test_eq: int] 260
    (part3
       [
         "1:7,1,9,1,6,9,8,3,7,2";
         "2:6,1,9,2,9,8,8,4,3,1";
         "3:7,1,9,1,6,9,8,3,8,3";
         "4:6,1,9,2,8,8,8,4,3,1";
         "5:7,1,9,1,6,9,8,3,7,3";
         "6:6,1,9,2,8,8,8,4,3,5";
         "7:3,7,2,2,7,4,4,6,3,1";
         "8:3,7,2,2,7,4,4,6,3,7";
         "9:3,7,2,2,7,4,1,6,3,7";
       ])

let%test_unit "part3 (given, second)" =
  [%test_eq: int] 4 (part3 [ "1:7,1,9,1,6,9,8,3,7,2"; "2:7,1,9,1,6,9,8,3,7,2" ])
