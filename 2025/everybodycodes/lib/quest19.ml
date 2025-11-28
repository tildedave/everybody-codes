[@@@ocaml.warning "-33"]

open Base
open Core
open Util

let parse_segments s =
  match List.map ~f:Int.of_string @@ String.split ~on:',' s with
  | [ x; ystart; yheight ] -> (x, (ystart, ystart + yheight))
  | _ -> failwith "invalid input"

let neighbors ((x, y), flap_count) wall_finder =
  let walls = wall_finder x in
  let next =
    [ ((x + 1, y - 1), flap_count); ((x + 1, y + 1), flap_count + 1) ]
  in
  match walls with
  | [] -> []
  | (wx, _) :: _ ->
      if wx = x + 1 then
        List.filter next ~f:(fun ((_, y), _) ->
            Option.is_some
              (List.find walls ~f:(fun (_, (sy, ey)) -> sy <= y && y < ey)))
      else next

let find_next_walls walls =
  let find x =
    let prefixes = walls |> List.drop_while ~f:(fun (wx, _) -> x >= wx) in
    match prefixes with
    | [] -> []
    | (wx, _) :: _ -> List.take_while prefixes ~f:(fun (wx', _) -> wx = wx')
  in
  Memo.general find
(* binary search *)

let flap l =
  let walls = List.map l ~f:parse_segments in
  let last_segment = fst @@ List.last_exn walls in
  let wall_finder = find_next_walls walls in
  let queue = Queue.create () in
  Queue.enqueue queue ((0, 0), 0);
  let best_flaps = ref Int.max_value in
  let best_for_spot = Hashtbl.create (module IntPair) in
  while not (Queue.is_empty queue) do
    let next = Queue.dequeue_exn queue in
    (* Stdio.printf "%s %d\n%!" (show_tuple (fst next)) (snd next); *)
    match next with
    | coords, flap_count ->
        if fst coords = last_segment then
          best_flaps := min !best_flaps (snd next);
        let current_best = Hashtbl.find best_for_spot coords in
        if
          match current_best with
          | None -> false
          | Some best -> flap_count >= best
        then ()
        else (
          Hashtbl.set best_for_spot ~key:coords ~data:flap_count;
          List.iter (neighbors next wall_finder) ~f:(Queue.enqueue queue))
    (* ()
       els
       if flap_count > best then (
         Stdio.printf "pruned (%d, %d) [%d flaps, best was %d]" (fst coords)
           (snd coords) flap_count best;
         ())
       else *)
    (* (match next with
       | (x, y), flap_count -> ; *)
  done;
  !best_flaps
