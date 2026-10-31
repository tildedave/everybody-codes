[@@@ocaml.warning "-31-32"]

open Base
open Util
open Core

let moves_from (max_x, max_y) (x, y) =
  List.filter
    ~f:(fun (x, y) -> x >= 0 && x < max_x && y >= 0 && y < max_y)
    [
      (x + 1, y + 2);
      (x - 1, y + 2);
      (x + 1, y - 2);
      (x - 1, y - 2);
      (x + 2, y + 1);
      (x + 2, y - 1);
      (x - 2, y + 1);
      (x - 2, y - 1);
    ]

let timedmoves_from bounds (n, (x, y)) =
  if equal n 0 then []
  else List.map ~f:(fun c -> (n - 1, c)) (moves_from bounds (x, y))

let char_at l (x, y) = String.get (List.nth_exn l y) x

let fold m ~init ~f ~neighbors start =
  let queue, visited, acc = (Queue.create (), Hash_set.create m, ref init) in
  Queue.enqueue queue start;
  while not (Queue.is_empty queue) do
    let next = Queue.dequeue_exn queue in
    if not (Hash_set.mem visited next) then (
      Hash_set.add visited next;
      acc := f !acc next;
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem visited neighbor) then
            Queue.enqueue queue neighbor)
        (neighbors next))
  done

module SearchState = struct
  type t = int * (int * int) [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

(* this works but is overkill I guess.  we visit the same nodes separately because the time is in the state.  but whatever. *)

let bounds l = (String.length (List.hd_exn l), List.length l)

let find_start l =
  l
  |> List.find_mapi ~f:(fun y s ->
         match String.index s 'D' with None -> None | Some x -> Some (x, y))
  |> Option.value_exn

let _part1 num_moves l =
  let hits = Hash_set.create (module IntPair) in
  fold
    (module SearchState)
    ~init:()
    ~f:(fun _ (_, loc) ->
      if equal_char (char_at l loc) 'S' then Hash_set.add hits loc)
    ~neighbors:(timedmoves_from (bounds l))
    (num_moves, find_start l);
  Hash_set.length hits

let part1 = _part1 4

let%test_unit "part1 (given)" =
  [%test_eq: int] 27
    (_part1 3
       [
         "...SSS.......";
         ".S......S.SS.";
         "..S....S...S.";
         "..........SS.";
         "..SSSS...S...";
         ".....SS..S..S";
         "SS....D.S....";
         "S.S..S..S....";
         "....S.......S";
         ".SSS..SS.....";
         ".........S...";
         ".......S....S";
         "SS.....S..S..";
       ])

(* immediately punished for doing something general, oh well *)
(* moving down is easy at least *)

let filter_lines ch =
  List.map ~f:(fun s ->
      String.to_array
      @@ String.map s ~f:(fun ch' -> if equal_char ch' ch then ch else '.'))

let filter_hideouts l = Array.of_list @@ filter_lines '#' l
let filter_sheep l = Array.of_list @@ filter_lines 'S' l

(* main worry is move generation will be too much but I guess it is bounded at the entire grid. *)

let kill_sheep hideouts sheep dragon_locations =
  let num_killed = ref 0 in
  Set.iter dragon_locations ~f:(fun (x, y) ->
      if equal_char sheep.(y).(x) 'S' && not (equal_char hideouts.(y).(x) '#')
      then (
        Array.set sheep.(y) x '.';
        num_killed := !num_killed + 1));
  !num_killed

let move_dragon bounds dragon_locations =
  Set.fold
    ~init:(Set.empty (module IntPair_Comparator))
    ~f:(fun acc coords -> set_add_all acc (moves_from bounds coords))
    dragon_locations

let move_sheep_down xmax sheep =
  Array.mapi
    ~f:(fun y _ ->
      if equal y 0 then Array.create ~len:xmax '.' else sheep.(y - 1))
    sheep

let _part2 num_rounds l =
  let xmax, ymax = bounds l in
  let dragon_locations =
    ref @@ Set.add (Set.empty (module IntPair_Comparator)) (find_start l)
  in
  let hideouts, sheep = (filter_hideouts l, ref @@ filter_sheep l) in
  let num_killed = ref 0 in
  for _ = 1 to num_rounds do
    num_killed := !num_killed + kill_sheep hideouts !sheep !dragon_locations;
    dragon_locations := move_dragon (xmax, ymax) !dragon_locations;
    num_killed := !num_killed + kill_sheep hideouts !sheep !dragon_locations;
    sheep := move_sheep_down xmax !sheep;
    num_killed := !num_killed + kill_sheep hideouts !sheep !dragon_locations
  done;
  !num_killed

let part2 = _part2 20

let%test_unit "part2 (given)" =
  [%test_eq: int] 27
    (_part2 3
       [
         "...SSS##.....";
         ".S#.##..S#SS.";
         "..S.##.S#..S.";
         ".#..#S##..SS.";
         "..SSSS.#.S.#.";
         ".##..SS.#S.#S";
         "SS##.#D.S.#..";
         "S.S..S..S###.";
         ".##.S#.#....S";
         ".SSS.#SS..##.";
         "..#.##...S##.";
         ".#...#.S#...S";
         "SS...#.S.#S..";
       ])
(* part 3 won't have a lot of reuse from the other parts.

   Game State: Sheep, Dragon location
   Constants:  Bounds, Hideouts

   Feels like bitboard time for the sheep, all examples are small enough
*)

let idx (xmax, _) (x, y) = (y * xmax) + x
let set_bit bb i = (1 lsl i) lor bb
let unset_bit bb i = lnot (1 lsl i) land bb
let is_bit_set bb i = not @@ equal ((bb lsr i) land 1) 0

let bb_to_string (xmax, ymax) bb =
  String.of_char_list
  @@ List.map
       ~f:(fun i -> if is_bit_set bb i then '#' else '.')
       (0 -- ((xmax * ymax) - 1))

let to_bb ch l =
  String.concat l
  |> String.foldi ~init:0 ~f:(fun n acc ch' ->
         if equal_char ch ch' then set_bit acc n else acc)

let%test_unit "bb to string" =
  [%test_eq: string] ".##.#........................."
    (let l = [ ".SS.S"; "#...#"; "...#."; "##..#"; ".####"; "##D.#" ] in
     bb_to_string (bounds l) (to_bb 'S' l))

let bb_fold_bits ~init ~f bb =
  let rec loop bb acc ~f =
    if bb = 0 then acc
    else
      (* bitboards are "reversed" from visual inspection, this avoids having
         to worry about padding *)
      let shift = Int.ctz bb in
      loop (unset_bit bb shift) (f shift acc) ~f
  in
  loop bb init ~f

let bb_iter ~f bb =
  let rec loop bb ~f =
    if bb = 0 then ()
    else
      (* bitboards are "reversed" from visual inspection, this avoids having
         to worry about padding *)
      let shift = Int.ctz bb in
      f shift;
      loop (unset_bit bb shift) ~f
  in
  loop bb ~f

(* each sheep can move down one *)
let next_sheep_boards (xmax, ymax) ~sheep ~dragon ~hideouts =
  bb_fold_bits ~init:(false, [])
    ~f:(fun n (has_win, acc) ->
      let next_idx = n + xmax in
      (* First conditional indicates a sheep escaped and so it shouldn't be
         considered in our seach tree *)
      if next_idx > (xmax * ymax) - 1 then (true, acc)
      else if next_idx = dragon && not (is_bit_set hideouts next_idx) then
        (has_win, acc)
      else (has_win, set_bit (unset_bit sheep n) next_idx :: acc))
    sheep

(* OK looks right *)

let dragon_moves_from (max_x, max_y) i =
  List.map
    ~f:(fun b -> idx (max_x, max_y) b)
    (moves_from (max_x, max_y) (i % max_x, i / max_x))

let eat_sheep ~sheep ~hideouts ~dragon =
  if is_bit_set hideouts dragon then sheep else unset_bit sheep dragon

let%test_unit "dragon moves from" =
  [%test_eq: int list] [ 7; 11 ]
    (List.sort ~compare (dragon_moves_from (5, 6) 0))

(* solution will then be dynamic programming with the hashtbl of int [sheep] * int [dragon position] *)

(* every sheep is on the final row of the board*)
let losing_state_bb (xmax, ymax) =
  List.fold ~init:0
    ~f:(fun bb n -> set_bit bb (idx (xmax, ymax) (n, ymax - 1)))
    (0 -- (xmax - 1))

let is_losing_state (xmax, ymax) sheep =
  with_return (fun r ->
      bb_iter
        ~f:(fun n -> if n < xmax * (ymax - 1) then r.return false else ())
        sheep;
      true)

let%test_unit "is_losing_state" =
  [%test_eq: bool] true
    (let l = [ "..."; "..."; "..."; "..."; "###" ] in
     is_losing_state (bounds l) (to_bb 'S' l))

let part3 l =
  let table, bounds = (Hashtbl.create (module IntPair), bounds l) in
  let initial_sheep, hideouts = (to_bb 'S' l, to_bb '#' l) in
  let losing_bb = losing_state_bb bounds in
  let dragon_start = idx bounds @@ find_start l in
  let rec helper sheep dragon =
    match Hashtbl.find table (sheep, dragon) with
    | Some n -> n
    | None ->
        let result =
          if sheep = 0 then 1
          else if losing_bb land sheep = sheep then 0
          else
            let has_win, sheep_moves =
              next_sheep_boards bounds ~sheep ~dragon ~hideouts
            in
            if List.is_empty sheep_moves then
              if has_win then 0
              else
                List.fold ~init:0
                  ~f:(fun acc next_dragon ->
                    acc
                    + helper
                        (eat_sheep ~sheep ~hideouts ~dragon:next_dragon)
                        next_dragon)
                  (dragon_moves_from bounds dragon)
            else
              List.fold ~init:0
                ~f:(fun acc (next_sheep, next_dragon) ->
                  acc
                  + helper
                      (eat_sheep ~sheep:next_sheep ~hideouts ~dragon:next_dragon)
                      next_dragon)
                (List.cartesian_product sheep_moves
                   (dragon_moves_from bounds dragon))
        in
        Hashtbl.set table ~key:(sheep, dragon) ~data:result;
        result
  in
  helper initial_sheep dragon_start
