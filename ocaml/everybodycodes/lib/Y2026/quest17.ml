open Base
open Util
open Search

let can_walk_to g coord = equal_char '#' (grid_at g coord)

let part1 l =
  let g = to_grid l in
  let xmax, ymax = g.bounds in
  dijkstra
    (module IntPair)
    (0, 0)
    ~is_goal:(fun (_, coord) -> equal_tuple (xmax - 1, ymax - 1) coord)
    ~neighbors:(fun coord ->
      Sequence.of_list
      @@ List.filter ~f:(can_walk_to g) (grid_cardinal_neighbors g coord))
    ~should_cutoff:(fun _ -> false)
  |> fst |> Option.value_exn

let example =
  [
    "####.######";
    "#.#.##..#..";
    "##.##.##..#";
    "#.#.###.###";
    "#.###.###.#";
    "###.##.#.##";
  ]

let%test_unit "part1" = [%test_result: int] ~expect:21 (part1 example)

let part2 l =
  let g = to_grid l in
  grid_foldi g ~init:0 ~f:(fun coord acc ch ->
      if equal_char ch '#' then
        acc + 4
        - List.count ~f:(can_walk_to g) (grid_cardinal_neighbors g coord)
      else acc)

let%test_unit "part2" = [%test_result: int] ~expect:94 (part2 example)

(* as expected we have to actually do something for part 3 *)
(* I don't think this is so bad *)

type wall_orientation = Left | Top | Right | Bottom
[@@deriving compare, show, sexp_of, hash, equal]

module Position = struct
  type t = tuple * wall_orientation
  [@@deriving compare, show, sexp_of, hash, equal]
end

(* each position only has two neighbors: hugging the left or right
   wall, you go up or down, hugging the bottom or top wall you go left or right *)

let add2 (x, y) (dx, dy) = (x + dx, y + dy)

let opposite orientation =
  match orientation with
  | Left -> Right
  | Top -> Bottom
  | Bottom -> Top
  | Right -> Left
(*
let turn_right orientation =
  match orientation with
  | Left -> Bottom
  | Right -> Top
  | Bottom -> Right
  | Top -> Left

let turn_left orientation =
  match orientation with
  | Left -> Top
  | Right -> Bottom
  | Bottom -> Left
  | Top -> Right *)

let position_neighbors orientation =
  match orientation with
  | Left -> ((Top, (0, -1), (-1, -1)), (Bottom, (0, 1), (-1, 1)))
  | Right -> ((Top, (0, -1), (1, -1)), (Bottom, (0, 1), (1, 1)))
  | Bottom -> ((Left, (-1, 0), (-1, 1)), (Right, (1, 0), (1, 1)))
  | Top -> ((Left, (-1, 0), (-1, -1)), (Right, (1, 0), (1, -1)))

let follow_turn g (coord, orientation) (next, next_delta, turn_delta) =
  let forward = add2 coord next_delta in
  match grid_at_opt g forward with
  | None | Some '.' ->
      (* Stdio.printf
        "can't move forward to %s, stay in the same coord and swap orientation \
         to %s\n"
        (show_tuple forward)
        (show_wall_orientation next); *)
      (coord, next)
      (* can't move forward, stay in same coord *)
  | Some '#' -> (
      (* can move forward, might bend *)
      (* Stdio.printf "can move forward, might bend\n"; *)
      match grid_at_opt g (add2 coord turn_delta) with
      | None | Some '.' ->
          (add2 coord next_delta, orientation)
          (* can't turn, same orientation *)
      | Some '#' -> (add2 coord turn_delta, opposite next) (* turn, and flip *)
      | _ -> failwith "impossible")
  | _ -> failwith "impossible"

let neighbors g (coord, orientation) =
  let f, b = position_neighbors orientation in
  let result =
    [
      follow_turn g (coord, orientation) f; follow_turn g (coord, orientation) b;
    ]
  in
  (* Stdio.printf "neighbors of %s ->> %s\n"
    (Position.show (coord, orientation))
    (String.concat ~sep:"," (List.map ~f:Position.show result)); *)
  Sequence.of_list result

let%test_unit "neighbors (top)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((1, 2), Top)))
    ~expect:[ ((0, 1), Right); ((1, 2), Right) ]

let%test_unit "neighbors (top - straight)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((5, 0), Top)))
    ~expect:[ ((5, 0), Left); ((6, 0), Top) ]

let%test_unit "neighbors (bottom)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((1, 2), Bottom)))
    ~expect:[ ((0, 3), Right); ((1, 2), Right) ]

let%test_unit "neighbors (bottom - straight)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((6, 0), Bottom)))
    ~expect:[ ((5, 1), Right); ((7, 0), Bottom) ]

let%test_unit "neighbors (left)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((2, 3), Left)))
    ~expect:[ ((2, 3), Top); ((2, 4), Left) ]

let%test_unit "neighbors (left - straight)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((6, 0), Bottom)))
    ~expect:[ ((5, 1), Right); ((7, 0), Bottom) ]

let%test_unit "neighbors (right)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((2, 3), Right)))
    ~expect:[ ((2, 3), Top); ((3, 4), Top) ]

let%test_unit "neighbors (right - straight)" =
  let g = to_grid example in
  [%test_result: Position.t list]
    (Sequence.to_list (neighbors g ((0, 4), Right)))
    ~expect:[ ((0, 3), Right); ((1, 5), Top) ]

(*

  match wall_orientation with
  | Left ->
      [
        (let above = add2 coord (0, -1) in
         match grid_at_opt g above with
         | None | Some '.' -> (coord, Top) (* can't move up *)
         | Some '#' -> (
             (* can move up, might move left *)
             match grid_at_opt g (add2 coord (-1, -1)) with
             | None | Some '.' -> (add2 coord (0, -1), Left)
             | Some '#' ->
                 (add2 coord (-1, -1), Bottom)
                 (* move to the bottom of this one *)
             | _ -> failwith "impossible")
         | _ -> failwith "impossible");
        (let below = add2 coord (0, 1) in
         match grid_at_opt g below with
         | None | Some '.' -> (coord, Bottom) (* can't move up *)
         | Some '#' -> (
             (* can move down, might move left *)
             match grid_at_opt g (add2 coord (-1, 1)) with
             | None | Some '.' -> (add2 coord (0, 1), Left)
             | Some '#' ->
                 (add2 coord (-1, -1), Top) (* move to the bottom of this one *)
             | _ -> failwith "impossible")
         | _ -> failwith "impossible");
      ] *)
(* OK now do this three more times :-) *)

(* top neighbor *)

(* dijkstra is actually silly since this is just DFS from two different direections *)

let p3_search l =
  let g = to_grid l in
  let xmax, ymax = g.bounds in
  let left, top = (((0, 0), Left), ((0, 0), Top)) in
  List.map
    ~f:(fun start ->
      dijkstra
        (module Position)
        start
        ~is_goal:(fun (_, (coord, orientation)) ->
          equal_tuple (xmax - 1, ymax - 1) coord
          && (equal_wall_orientation orientation Bottom
             || equal_wall_orientation orientation Right))
        ~neighbors:(fun coord ->
          let n = neighbors g coord in
          if Position.equal left start && Position.equal coord left then
            Sequence.filter ~f:(fun c -> not (Position.equal c top)) n
          else if Position.equal top start && Position.equal coord top then
            Sequence.filter ~f:(fun c -> not (Position.equal c left)) n
          else n)
        ~should_cutoff:(fun _ -> false)
      |> fst |> Option.value_exn |> ( + ) 1)
    [ left; top ]

let part3 l = p3_search l |> List.min_elt ~compare |> Option.value_exn

let%test_unit "p3_search" =
  [%test_result: int list] ~expect:[ 31; 63 ] (p3_search example)

let%test_unit "part3" = [%test_result: int] ~expect:31 (part3 example)
