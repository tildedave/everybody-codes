open Base
open Util

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

module Coord = struct
  type t = int * int [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

module Coord_Comparator = struct
  include Coord
  include Base.Comparator.Make (Coord)
end

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
  let hits = Hash_set.create (module Coord) in
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
    ~init:(Set.empty (module Coord_Comparator))
    ~f:(fun acc coord -> set_add_all acc (moves_from bounds coord))
    dragon_locations

let move_sheep_down xmax sheep =
  Array.mapi
    ~f:(fun y _ ->
      if equal y 0 then Array.create ~len:xmax '.' else sheep.(y - 1))
    sheep

let _part2 num_rounds l =
  let xmax, ymax = bounds l in
  let dragon_locations =
    ref @@ Set.add (Set.empty (module Coord_Comparator)) (find_start l)
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
