[@@@ocaml.warning "-32-33"]

open Base
open Util

let part2 _ = 1

(* part 3 seems straightforward, but we will need to maintain a "rotation" *)
(* https://www.redblobgames.com/grids/parts/#triangle-grids *)
(* seems like we should try to just convert the whole thing to triangle coords,
   rotation will be much easier *)
type parity = Left | Right [@@deriving eq, show, compare, sexp_of]
type triangle_coord = int * int * parity [@@deriving eq, compare, sexp_of, show]

module TriangleCoord = struct
  type t = triangle_coord [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

module TriangleCoord_Comparator = struct
  include TriangleCoord
  include Base.Comparator.Make (TriangleCoord)
end

let triangle_neighbors t (q, r, parity) =
  let neighbors =
    match parity with
    | Right -> [ (q, r, Left); (q + 1, r, Left); (q, r + 1, Left) ]
    | Left -> [ (q, r - 1, Right); (q - 1, r, Right); (q, r, Right) ]
  in
  List.filter neighbors ~f:(Map.mem t)

let create_triangle_grid l =
  List.foldi
    ~init:(Map.empty (module TriangleCoord_Comparator))
    ~f:(fun r acc s ->
      let m = ref acc in
      let seen_beginning = ref false in
      let polarity = ref Left in
      let q = ref 0 in
      for x = 0 to String.length s - 1 do
        if (not !seen_beginning) && not (equal_char s.[x] '.') then
          seen_beginning := true;
        if !seen_beginning then (
          m := Map.add_exn !m ~key:(!q, r, !polarity) ~data:s.[x];
          match !polarity with
          | Left -> polarity := Right
          | Right ->
              polarity := Left;
              q := !q + 1)
      done;
      !m)
    l

let can_jump triangle coords ncoords =
  let from_char = Map.find_exn triangle coords in
  let to_char = Map.find_exn triangle ncoords in
  if not (equal_char from_char 'T' || equal_char from_char 'S') then false
  else if not (equal_char to_char 'T' || equal_char to_char 'E') then false
  else true

let part1 l =
  let t = create_triangle_grid l in
  let result =
    Map.fold ~init:0
      ~f:(fun ~key:coords ~data:_ acc ->
        List.fold ~init:acc
          ~f:(fun acc ncoords ->
            if can_jump t coords ncoords then acc + 1 else acc)
          (triangle_neighbors t coords))
      t
  in
  result / 2

let find_triangle_coord t ch =
  Map.fold_until ~init:None
    ~f:(fun ~key ~data _ ->
      let open Continue_or_stop in
      if equal_char data ch then Stop (Some key) else Continue None)
    ~finish:(fun _ -> None)
    t

let part2 l =
  let t = create_triangle_grid l in
  let queue = Queue.create () in
  let start = find_triangle_coord t 'S' |> Option.value_exn in
  let goal = find_triangle_coord t 'E' |> Option.value_exn in
  let distance = Hashtbl.create (module TriangleCoord) in
  let explored = Hash_set.create (module TriangleCoord) in
  Queue.enqueue queue start;
  Hashtbl.set distance ~key:start ~data:0;
  Hash_set.add explored start;
  let loop_done = ref false in
  while (not (Queue.is_empty queue)) && not !loop_done do
    let next = Queue.dequeue_exn queue in
    if equal_triangle_coord next goal then loop_done := true
    else
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem explored neighbor) then (
            Hash_set.add explored neighbor;
            Hashtbl.set distance ~key:neighbor
              ~data:(Hashtbl.find_exn distance next + 1);
            Queue.enqueue queue neighbor))
        (List.filter ~f:(can_jump t next) (triangle_neighbors t next))
  done;
  Hashtbl.find_exn distance goal

let%test_unit "compare triangle coord" =
  [%test_eq: int] (-1) (compare_triangle_coord (0, 0, Left) (0, 0, Right))

let%test_unit "compare triangle coord" =
  [%test_eq: int] (-1) (compare_triangle_coord (0, 0, Left) (0, 0, Right))

(* OK now we just code triangle rotation.  it does depend on the size of
   the triangle *)

let endpoints t =
  (* find highest and lowest q for r = 0, find highest r *)
  match
    Map.fold ~init:(None, None, None)
      ~f:(fun ~key:tcoord ~data:_ (left, right, bottom) ->
        match tcoord with
        | _, r, _ ->
            ( (if r <> 0 then left
               else
                 match left with
                 | None -> Some tcoord
                 | Some left' ->
                     if compare_triangle_coord tcoord left' = -1 then
                       Some tcoord
                     else left),
              (if r <> 0 then right
               else
                 match right with
                 | None -> Some tcoord
                 | Some right' ->
                     if compare_triangle_coord tcoord right' = 1 then
                       Some tcoord
                     else right),
              match bottom with
              | None -> Some tcoord
              | Some (_, r', _) -> if r > r' then Some tcoord else bottom ))
      t
  with
  | Some left, Some right, Some bottom -> (left, right, bottom)
  | _ -> failwith "invalid parsing"

let example_triangle =
  [
    "T####T#TTT##T##T#T#";
    ".T#####TTTT##TTT##.";
    "..TTTT#T###TTTT#T..";
    "...T#TTT#ETTTT##...";
    "....#TT##T#T##T....";
    ".....#TT####T#.....";
    "......T#TT#T#......";
    ".......T#TTT.......";
    "........TT#........";
    ".........S.........";
  ]

let%test_unit "endpoints" =
  [%test_eq: triangle_coord * triangle_coord * triangle_coord]
    ((0, 0, Left), (9, 0, Left), (0, 9, Left))
    (endpoints (create_triangle_grid example_triangle))

let%test_unit "midpoint" =
  [%test_eq: triangle_coord] (3, 3, Left)
    (Option.value_exn
       (find_triangle_coord (create_triangle_grid example_triangle) 'E'))

(*
OK so example:
    0,0L 1,0L 2,0L 3,0L 4,0L
      0,0R 1,0R 2,0R 3,0R
      0,1L 1,1L 2,1L 3,1L
         0,1R 1,1R 2,1R
         0,2L 1,2L 2,2L
           0,2R 1,2R
           0,3L 1,3L
              0,3R
              0,4L

0,0L -> 4,0L
4,0L -> 0,4L
0,4L -> 0,0L

*)

(* can construct a rotation matrix by transforming the right-to-left lines (0,0L) into down lines *)
(* 0,0L -> 0,0R -> 1,0L, 1,0R -> ...  *)
(* into 2,0L -> 1,0R -> 1,1L -> 0,1R -> 0,2L *)
(* does this for each of the edges, then continue with the interior triangles *)
(* this seems like a giant pain to code .... maybe it will be fine *)

let walk_right coord goal =
  let open Sequence.Step in
  Sequence.unfold_step ~init:coord ~f:(fun (q, r, polarity) ->
      let next =
        match polarity with Left -> (q, r, Right) | Right -> (q + 1, r, Left)
      in
      if equal_triangle_coord next goal then Done
      else Yield { value = next; state = next })

let walk_up_left coord goal =
  let open Sequence.Step in
  Sequence.unfold_step ~init:coord ~f:(fun (q, r, polarity) ->
      let next =
        match polarity with Left -> (q, r - 1, Right) | Right -> (q, r, Left)
      in
      if equal_triangle_coord next goal then Done
      else Yield { value = next; state = next })

let walk_down_left coord goal =
  let open Sequence.Step in
  Sequence.unfold_step ~init:coord ~f:(fun (q, r, polarity) ->
      let next =
        match polarity with
        | Left -> (q - 1, r, Right)
        | Right -> (q, r + 1, Left)
      in
      if equal_triangle_coord next goal then Done
      else Yield { value = next; state = next })

let rotation_map t =
  let rec loop (left, right, bottom) map n =
    Stdio.printf "triangle formed by %s %s %s%!\n" (show_triangle_coord left)
      (show_triangle_coord right)
      (show_triangle_coord bottom);
    (* issue: the recursive step will need to stop at a cetain point,
       how to determine this *)
    let next_map =
      Sequence.fold ~init:map ~f:(fun map (source, dest) ->
          Stdio.printf "source -> %s goes to dest -> %s%!\n"
            (show_triangle_coord source)
            (show_triangle_coord dest);
          Map.set map ~key:source ~data:dest)
      @@ Sequence.concat
      @@ Sequence.of_list
           [
             Sequence.zip (walk_right left right) (walk_down_left right bottom);
             Sequence.zip
               (walk_down_left right bottom)
               (walk_up_left bottom left);
             Sequence.zip (walk_up_left bottom left) (walk_right left right);
           ]
    in
    match (left, right, bottom) with
    | (leftq, leftr, _), (rightq, rightr, _), (bottomq, bottomr, _) ->
        let next_left, next_right, next_bottom =
          ( (leftq + 1, leftr + 1, Left),
            (rightq - 2, rightr + 1, Left),
            (bottomq + 1, bottomr - 2, Left) )
        in
        if
          Map.mem next_map next_left
          || Map.mem next_map next_right
          || Map.mem next_map next_bottom
          || (not (Map.mem t next_right))
          || (not (Map.mem t next_left))
          || not (Map.mem t next_bottom)
        then map
        else if n < 3 then loop (next_left, next_right, next_bottom) map (n + 1)
        else map
  in
  loop (endpoints t) (Map.empty (module TriangleCoord_Comparator)) 0

(* mapping comes from this: *)
(* then find new midpoints and recurse... *)

let%test_unit "rotation_map" =
  [%test_eq: int] 0
    (Map.length (rotation_map (create_triangle_grid example_triangle)))
