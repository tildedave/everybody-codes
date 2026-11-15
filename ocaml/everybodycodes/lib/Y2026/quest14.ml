[@@@ocaml.warning "-27-26-31-32"]

open Base
open Util

let example =
  [ "AAAEEEFFFF"; "AADDDDDGFF"; "BACDDIGGFF"; "BABBIIIGGH"; "BBBBIIIIHH" ]

(* I think we can just double count every connection / 2 then find grid bounds *)

let count_connections g =
  List.fold ~init:0
    ~f:(fun acc coord ->
      let coord_ch = grid_at g coord in
      List.fold ~init:acc
        ~f:(fun acc neighbor ->
          if not (equal_char (grid_at g neighbor) coord_ch) then acc + 1
          else acc)
        (grid_cardinal_neighbors g coord))
    (grid_all_coords g)

let part1 l =
  let g = to_grid l in
  let xmax, ymax = g.bounds in
  (count_connections g / 2) + (2 * xmax) + (2 * ymax)

let%test_unit "part1" = [%test_eq: int] 67 (part1 example)

(* part2 -- we have to pick what an edge looks like, let's
   say x = LEFT edge, y = BOTTOM edge, then we need to iterate from
   y = -1 -> y, x = 0 -> xmax (inclusive).  for each component we keep track
   of edge coords then we can organize by x/y and then build a MST.  *)
module CharOption = struct
  type t = char option [@@deriving compare, sexp_of, equal]

  let hash = Hashtbl.hash
end

module CharOption_Comparator = struct
  include CharOption
  include Base.Comparator.Make (CharOption)
end

let add_if_not_matching g coord other_coord m =
  match (grid_at_opt g coord, grid_at_opt g other_coord) with
  | None, Some ch -> Map.add_multi m ~key:None ~data:(Some ch, other_coord)
  | Some ch, None -> Map.add_multi m ~key:(Some ch) ~data:(None, other_coord)
  | None, None -> m
  | Some ch, Some ch' ->
      if equal_char ch ch' then m
      else Map.add_multi m ~key:(Some ch) ~data:(Some ch', other_coord)

let find_edges g =
  let xmax, ymax = g.bounds in
  let indices =
    let open Sequence.Let_syntax in
    let%bind y = Sequence.range (-1) ymax in
    let%map x = Sequence.range 0 xmax ~start:`inclusive in
    (x, y)
  in
  (* I suppose we can't use the neighbor logic *)
  Sequence.fold
    ~init:
      ( Map.empty (module CharOption_Comparator),
        Map.empty (module CharOption_Comparator) )
    ~f:(fun (horizontal_m, vertical_m) coord ->
      let cx, cy = coord in
      ( horizontal_m |> add_if_not_matching g coord (cx - 1, cy),
        vertical_m |> add_if_not_matching g coord (cx, cy + 1) ))
    indices

(* now we group horizontal and vertical edges together *)

let _print_map label =
  Map.iteri ~f:(fun ~key ~data ->
      Stdio.printf "[%s] %c --> %s\n" label key
        (String.concat ~sep:","
           (List.map
              ~f:(fun (ch_opt, coord) ->
                Printf.sprintf "(%s, %s)"
                  (match ch_opt with
                  | None -> "<void>"
                  | Some ch -> Char.to_string ch)
                  (show_tuple coord))
              data)))

(* OK construct edges between the components and then run a MST algo *)

(* graph is an adjacency list *)

let add_edges m ~proj graph =
  Map.fold ~init:graph
    ~f:(fun ~key ~data graph ->
      (* these are all the edges from a component to another, with the coord *)
      let component_map =
        Map.of_alist_multi (module CharOption_Comparator) data
      in
      Map.fold ~init:graph
        ~f:(fun ~key:ch_opt ~data graph ->
          let cnts =
            List.fold
              ~init:(Map.empty (module Int))
              ~f:(fun acc coord ->
                Map.update acc (proj coord) ~f:(function
                  | None -> 1
                  | Some n -> n + 1))
              data
          in
          Map.fold ~init:graph
            ~f:(fun ~key:_ ~data:weight graph ->
              graph
              (* add both directions so we're undirected *)
              |> Map.add_multi ~key ~data:(ch_opt, weight)
              |> Map.add_multi ~key:ch_opt ~data:(key, weight))
            cnts)
        component_map)
    m

let show_component ch_opt =
  match ch_opt with None -> "<outside>" | Some ch -> Char.to_string ch

(* this seems to work *)
let _print_tree =
  Map.iteri ~f:(fun ~key ~data ->
      Stdio.printf "%s ---> %s\n" (show_component key)
        (String.concat ~sep:","
           (List.map
              ~f:(fun (ch_opt, weight) ->
                Printf.sprintf "(%s, %d)" (show_component ch_opt) weight)
              data)))

let part2 l =
  let g = to_grid l in
  let horizontal, vertical = find_edges g in
  let graph =
    Map.empty (module CharOption_Comparator)
    |> add_edges horizontal ~proj:fst
    |> add_edges vertical ~proj:snd
  in
  _print_tree graph;
  (* let _ = add_edges graph horizontal ~proj:fst in *)
  1

let%test_unit "part2" = [%test_eq: int] 12 (part2 example)
let part3 _ = 1
