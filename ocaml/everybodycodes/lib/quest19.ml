[@@@ocaml.warning "-33"]

open Base
open Core
open Util

let parse_segments s =
  match List.map ~f:Int.of_string @@ String.split ~on:',' s with
  | [ x; ystart; yheight ] -> (x, (ystart, ystart + yheight))
  | _ -> failwith "invalid input"

let flap_count (x, y) (destx, desty) =
  if (destx + desty) % 2 = 1 then None
  else if Int.abs (desty - y) > Int.abs (destx - x) then None
  else
    (* adjust to y, then maintain height *)
    let time = Int.abs (desty - y) in
    Some ((if desty > y then time else 0) + ((destx - x - time) / 2))

let%test_unit "flap_count (1)" =
  [%test_eq: int option] (Some 1) (flap_count (0, 0) (1, 1))

let%test_unit "flap_count (inadmissable)" =
  [%test_eq: int option] None (flap_count (0, 0) (1, 3))

let%test_unit "flap_count (2)" =
  [%test_eq: int option] (Some 1) (flap_count (0, 0) (2, 0))

let%test_unit "flap_count (3)" =
  [%test_eq: int option] (Some 7) (flap_count (0, 0) (7, 7))

let%test_unit "flap_count (4)" =
  [%test_eq: int option] (Some 0) (flap_count (7, 7) (12, 2))

let%test_unit "flap_count (5)" =
  [%test_eq: int option] (Some 4) (flap_count (15, 5) (24, 4))

let neighbors ((x, y), f) wall_finder : (tuple * int) list =
  let walls = wall_finder x in
  List.fold ~init:[]
    ~f:(fun acc (wx, (sy, ey)) ->
      List.append acc
        (List.filter_map
           ~f:(fun wy ->
             match flap_count (x, y) (wx, wy) with
             | None -> None
             | Some s -> Some ((wx, wy), f + s))
           (sy -- (ey - 1))))
    walls

let find_next_walls walls =
  let find x =
    let prefixes = walls |> List.drop_while ~f:(fun (wx, _) -> x >= wx) in
    match prefixes with
    | [] -> []
    | (wx, _) :: _ -> List.take_while prefixes ~f:(fun (wx', _) -> wx = wx')
  in
  Memo.general find

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
  done;
  !best_flaps

(* can maybe solve this with an angle approach *)
