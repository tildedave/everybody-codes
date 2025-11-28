open Base
open Util

let parse_segments s =
  match List.map ~f:Int.of_string @@ String.split ~on:',' s with
  | [ x; ystart; yheight ] -> (x, (ystart, ystart + yheight))
  | _ -> failwith "invalid input"

let neighbors ((x, y), flap_count) next_walls =
  match Hashtbl.find next_walls x with
  | None -> []
  | Some walls -> (
      (* Stdio.printf "(%d, %d) walls %s\n" x y
         (String.concat ~sep:","
            (List.map walls ~f:(fun (x, (sy, ey)) ->
                 Printf.sprintf "%d [%d --> %d]" x sy ey))); *)
      let next =
        [ ((x + 1, y - 1), flap_count); ((x + 1, y + 1), flap_count + 1) ]
      in
      match walls with
      | (wx, _) :: _ ->
          if wx = x + 1 then
            List.filter next ~f:(fun ((_, y), _) ->
                Option.is_some
                  (List.find walls ~f:(fun (_, (sy, ey)) -> sy <= y && y < ey)))
          else next
      | _ -> next)
(* else if y + (wx - x) < sy then []
   else if y - (wx - x) > ey then [] *)

let part1 l =
  let walls = List.map l ~f:parse_segments in
  let last_segment = walls |> List.last_exn |> fst in
  let next_walls = Hashtbl.create (module Int) in
  List.iteri
    ~f:(fun n v -> Hashtbl.add_exn next_walls ~key:n ~data:v)
    (List.init (last_segment + 1) ~f:(fun x ->
         walls
         |> List.drop_while ~f:(fun (wx, _) -> x > wx)
         |> List.take_while ~f:(fun (wx, _) -> x <= wx)));
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
          List.iter (neighbors next next_walls) ~f:(Queue.enqueue queue))
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

let part2 _ = 1
let part3 _ = 1
