open Base
open Util

let in_volcano_radius (xv, yv) (xc, yc) r =
  ((xv - xc) * (xv - xc)) + ((yv - yc) * (yv - yc)) <= r * r

let distance_to_volcano (xv, yv) (xc, yc) =
  let distsq = ((xv - xc) * (xv - xc)) + ((yv - yc) * (yv - yc)) in
  let r = isqrt @@ distsq in
  if distsq = r * r then r else r + 1

let num_destroyed r destroyed grid =
  let xv, yv = grid |> grid_find ~f:(equal_char '@') |> Option.value_exn in
  grid_fold grid ~init:(0, destroyed) ~f:(fun (xc, yc) (count, destroyed) ch ->
      if equal_tuple (xc, yc) (xv, yv) then (count, destroyed)
      else if Set.mem destroyed (xc, yc) then (count, destroyed)
      else if in_volcano_radius (xv, yv) (xc, yc) r then
        (count + num_of_char ch, Set.add destroyed (xc, yc))
      else (count, destroyed))

let destruction_sequence grid =
  Sequence.unfold
    ~init:(1, Set.empty (module IntPair_Comparator))
    ~f:(fun (r, destroyed) ->
      let count, next_destroyed = num_destroyed r destroyed grid in
      Some (count, (r + 1, next_destroyed)))

let part1 l =
  let grid = to_grid l in
  fst @@ num_destroyed 10 (Set.empty (module IntPair_Comparator)) grid

let%test_unit "part1 (given)" =
  [%test_eq: int] 1573
    (part1
       [
         "189482189843433862719";
         "279415473483436249988";
         "432746714658787816631";
         "428219317375373724944";
         "938163982835287292238";
         "627369424372196193484";
         "539825864246487765271";
         "517475755641128575965";
         "685934212385479112825";
         "815992793826881115341";
         "1737798467@7983146242";
         "867597735651751839244";
         "868364647534879928345";
         "519348954366296559425";
         "134425275832833829382";
         "764324337429656245499";
         "654662236199275446914";
         "317179356373398118618";
         "542673939694417586329";
         "987342622289291613318";
         "971977649141188759131";
       ])

let part2 l =
  let grid = to_grid l in
  destruction_sequence grid
  |> Sequence.take_while ~f:(fun n -> n <> 0)
  |> Sequence.mapi ~f:(fun n x -> (n + 1, x))
  |> Sequence.max_elt ~compare:(fun (_, x) (_, y) -> compare x y)
  |> Option.value_exn
  |> fun (x, y) -> x * y

let%test_unit "part2 (given)" =
  [%test_eq: int] 1090
    (part2
       [
         "4547488458944";
         "9786999467759";
         "6969499575989";
         "7775645848998";
         "6659696497857";
         "5569777444746";
         "968586@767979";
         "6476956899989";
         "5659745697598";
         "6874989897744";
         "6479994574886";
         "6694118785585";
         "9568991647449";
       ])

(* part3 is a* search *)
(* state is:
   position,
   time,
   number of quadrants visited (can force clockwise or ccw direction),
   closest radius to volcano
*)
(* then use precomputed volcano death times *)
(* I guess we can just DFS it for maximum laziness *)
(* DFS just finds a search, not the best search *)

type searchstate = {
  position : tuple;
  time : int;
  progress : int;
  closest_radius : int;
}
[@@deriving compare, sexp_of, show]

let _ = show_searchstate

module SearchState = struct
  type t = searchstate [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

module SearchState_order = struct
  include SearchState
  include Base.Comparator.Make (SearchState)
end

let quadrant (xv, yv) (xc, yc) =
  match
    (Ordering.of_int @@ compare xc xv, Ordering.of_int @@ compare yc yv)
  with
  | Equal, _ -> None
  | _, Equal -> None
  | Less, Less -> Some 1
  | Less, Greater -> Some 2
  | Greater, Greater -> Some 3
  | Greater, Less -> Some 4

let neighbors grid vcoords { position; time; progress; closest_radius } =
  List.filter_map
    ~f:(fun ncoords ->
      let next_quadrant = quadrant vcoords ncoords in
      let next_progress =
        match next_quadrant with
        | None -> progress
        | Some q -> if q = progress + 1 then progress + 1 else progress
      in
      (*
      if
        match next_quadrant with
        | Some q ->
            if progress = 4 then false else q <> progress + 1 && q <> progress
        | None -> false
      then (* going to the wrong section, or going backwards *)
        None
      else
        *)
      let time_used =
        match grid_at grid ncoords with 'S' -> 0 | ch -> num_of_char ch
      in
      let next =
        {
          position = ncoords;
          time = time + time_used;
          progress = next_progress;
          closest_radius =
            min closest_radius (distance_to_volcano vcoords ncoords);
        }
      in
      let volcano_radius = next.time / 30 in
      if next.closest_radius <= volcano_radius then None else Some next)
    (grid_cardinal_neighbors grid position)

(* also keep track of a "best time/progress for square" and prune based on that *)

module BestPosition = struct
  type t = tuple * int * int [@@deriving compare, sexp_of] (* progress *)

  let hash = Hashtbl.hash
end

module BestPosition_order = struct
  include BestPosition
  include Base.Comparator.Make (BestPosition)
end

let best_for_pos best_time node =
  Hashtbl.update_and_return best_time
    (node.position, node.closest_radius, node.progress) ~f:(fun curr ->
      match curr with
      | None -> node.time
      | Some best_so_far -> min node.time best_so_far)

let volcano_search grid =
  let volcano = grid |> grid_find ~f:(equal_char '@') |> Option.value_exn in
  let start = grid |> grid_find ~f:(equal_char 'S') |> Option.value_exn in
  let best_time = Hashtbl.create (module BestPosition_order) in
  let frontier_by_radius = Hashtbl.create (module Int) in
  let loop_done = ref false in
  let result = ref None in
  let best_radius = ref 0 in
  let radius_0 = Hash_set.create (module SearchState_order) in
  Hash_set.add radius_0
    {
      position = start;
      time = 0;
      progress = 0;
      closest_radius = distance_to_volcano start volcano;
    };
  Hashtbl.add_exn frontier_by_radius ~key:0 ~data:radius_0;
  while not !loop_done do
    if Hash_set.is_empty (Hashtbl.find_exn frontier_by_radius !best_radius) then (
      Stdio.printf "exhausted radius %d %!\n" !best_radius;
      best_radius := !best_radius + 1);
    let frontier = Hashtbl.find_exn frontier_by_radius !best_radius in
    let node =
      (* dijkstra - using a real priority queue would be nice here *)
      frontier
      |> Hash_set.fold ~init:(Int.max_value, None)
           ~f:(fun (minsofar, winner) curr ->
             if curr.time < minsofar then (curr.time, Some curr)
             else (minsofar, winner))
      |> snd |> Option.value_exn
    in
    Hash_set.remove frontier node;
    let best_time_for_pos = best_for_pos best_time node in
    if equal_tuple node.position start && node.progress = 4 then (
      result := Some node;
      loop_done := true)
    else if node.time > best_time_for_pos then ()
    else
      List.iter (neighbors grid volcano node) ~f:(fun neighbor ->
          (* prune neighbor by best time for pos*)
          let best_time_for_pos = best_for_pos best_time neighbor in
          if neighbor.time > best_time_for_pos then ()
          else
            let radius_time = neighbor.time / 30 in
            let table =
              Hashtbl.update_and_return frontier_by_radius radius_time
                ~f:(fun radius_set ->
                  match radius_set with
                  | None -> Hash_set.create (module SearchState_order)
                  | Some t -> t)
            in
            Hash_set.add table neighbor)
  done;
  Option.value_exn !result

let part3 l =
  let s = volcano_search (to_grid l) in
  s.time * (s.time / 30)
