[@@@ocaml.warning "-32-33"]

open Base
open Util

let part2 _ = 1

(* part 3 seems straightforward, but we will need to maintain a "rotation" *)
(* https://www.redblobgames.com/grids/parts/#triangle-grids *)
(* seems like we should try to just convert the whole thing to triangle coords,
   rotation will be much easier *)
type parity = Left | Right [@@deriving eq, show, compare, sexp_of]
type triangle_coord = int * int * parity [@@deriving eq, compare, sexp_of]

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
