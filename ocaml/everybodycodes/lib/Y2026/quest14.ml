open Base
open Util
open Core

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
end

module CharOption_Comparator = struct
  include CharOption
  include Base.Comparator.Make (CharOption)
end

module Edge = struct
  type t = char option * char option * int [@@deriving compare, sexp_of, equal]
end

module Edge_Comparator = struct
  include Edge
  include Base.Comparable.Make (Edge)
end

let increment m k1 k2 =
  Map.update m k1 ~f:(function
    | None -> Map.singleton (module CharOption_Comparator) k2 1
    | Some m' -> Map.update m' k2 ~f:(function None -> 1 | Some n -> n + 1))

let add_if_not_matching g coord other_coord m =
  match (grid_at_opt g coord, grid_at_opt g other_coord) with
  | None, Some ch -> increment m None (Some ch)
  | Some ch, None -> increment m (Some ch) None
  | None, None -> m
  | Some ch, Some ch' when equal_char ch ch' -> m
  | Some ch, Some ch' -> increment m (Some ch) (Some ch')

let find_edges g =
  let xmax, ymax = g.bounds in
  let indices =
    let open Sequence.Let_syntax in
    let%bind y = Sequence.range (-1) ymax ~start:`inclusive ~stop:`inclusive in
    let%map x = Sequence.range (-1) xmax ~start:`inclusive ~stop:`inclusive in
    (x, y)
  in
  (* I suppose we can't use the neighbor logic *)
  Sequence.fold
    ~init:(Map.empty (module CharOption_Comparator))
    ~f:(fun m coord ->
      let cx, cy = coord in
      m
      |> add_if_not_matching g coord (cx - 1, cy)
      |> add_if_not_matching g coord (cx, cy + 1)
      |> add_if_not_matching g coord (cx + 1, cy)
      |> add_if_not_matching g coord (cx, cy - 1))
    indices
(* |> Map.fold
       ~init:(Map.empty (module CharOption_Comparator))
       ~f:(fun ~key:u ~data m ->
         let component_map =
           Map.of_alist_multi (module CharOption_Comparator) data
         in
         Map.fold component_map ~init:m ~f:(fun ~key:v ~data:l m ->
             Map.add_multi m ~key:u ~data:(v, List.length l))) *)

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

(* MST algo: we'll use Boruvka's algo per Jeff Erickson.  Good ol UIUC *)

let count_components m_mst =
  m_mst
  |> Map.fold
       ~init:(Set.empty (module CharOption_Comparator))
       ~f:(fun ~key:_ ~data acc -> Set.add acc (Union_find.get data))
  |> Set.length

let edge_set graph =
  Map.fold
    ~init:(Set.empty (module Edge_Comparator))
    ~f:(fun ~key:u ~data edge_set ->
      Map.fold ~init:edge_set
        ~f:(fun ~key:v ~data:w edge_set -> Set.add edge_set (u, v, w))
        data)
    graph

let _mst_kruskal graph components =
  edge_set graph |> Set.to_list
  |> List.sort ~compare:(Comparable.lift compare ~f:(fun (_, _, w) -> w))
  |> List.fold
       ~init:(Set.empty (module Edge_Comparator))
       ~f:(fun minset (u, v, w) ->
         let u_uf, v_uf =
           (Map.find_exn components u, Map.find_exn components v)
         in
         if Union_find.same_class u_uf v_uf then minset
         else begin
           Union_find.union u_uf v_uf;
           Set.add minset (u, v, w)
         end)

let _mst_boruvka graph components =
  let rec helper minset =
    let count = count_components components in
    if count > 1 then
      let safe_edges =
        Map.fold
          ~init:(Map.empty (module CharOption_Comparator))
          ~f:(fun ~key:u ~data safe_edges ->
            List.fold ~init:safe_edges
              ~f:(fun safe_edges (v, w) ->
                (* test if key and ch_opt are in the same component *)
                let u_uf, v_uf =
                  (Map.find_exn components u, Map.find_exn components v)
                in
                if Union_find.same_class u_uf v_uf then safe_edges
                else
                  (* Stdio.printf "different class, time to add edges\n"; *)
                  ( safe_edges |> fun m ->
                    Map.update m (Union_find.get u_uf) ~f:(function
                      | None -> (u, v, w)
                      | Some (u', v', w') ->
                          if w < w' then (u, v, w) else (u', v', w')) )
                  |> fun m ->
                  Map.update m (Union_find.get v_uf) ~f:(function
                    | None -> (v, u, w)
                    | Some (v', u', w') ->
                        if w < w' then (v, u, w) else (v', u', w')))
              data)
          graph
      in
      (* now we add all the edges to the same class, then add the counts to
           the minset *)
      let next_minset =
        Map.fold ~init:minset safe_edges
          ~f:(fun ~key:_ ~data:(u, v, w) minset ->
            let u_uf, v_uf =
              (Map.find_exn components u, Map.find_exn components v)
            in
            if Set.mem minset (v, u, w) then minset
            else (
              Union_find.union u_uf v_uf;
              Set.add minset (u, v, w)))
      in
      helper next_minset
    else minset
  in
  helper (Set.empty (module Edge_Comparator))

let mst = _mst_kruskal

let to_uf_components graph =
  Map.of_alist_exn
    (module CharOption_Comparator)
    (List.map (Map.keys graph) ~f:(fun k -> (k, Union_find.create k)))

let sum_weights s = Set.sum (module Int) s ~f:(fun (_, _, w) -> w)

let part2 l =
  let g = to_grid l in
  let graph = find_edges g in
  sum_weights (mst graph (to_uf_components graph))

let%test_unit "part2" = [%test_result: int] ~expect:12 (part2 example)

let p2_example2 =
  [
    "IIIIIILOHHHHHHHHGGGGGQSRRRRKKK";
    "IIIIIILOOHHHHHHGGXGGGQSRRRRKKK";
    "PPPPILLLOOOODDHBGGGGGQSSRRRKKK";
    "PPZPLLLLFFOODDBBBBGGQQQSSRKKKK";
    "PPPPLLLLFFFONDDDBBMGGQQSSKKJJJ";
    "PPPCCCCCCFFONNDDMMMGQQQSSSKJJJ";
    "PCCCCCCCCFFNNDDDDMAAQQQSSSKKJJ";
    "PPCCECCCFFNNNNDADMAATQTTSSSJJJ";
    "PPEEEEECCFFNNAAAMMATTTTTSTSSSJ";
    "PPEEEEEEEFFNNNAAAAAATTTTTTSSJJ";
  ]

let%test_unit "part2 (second example)" =
  [%test_result: int] ~expect:63 (part2 p2_example2)

let spell_of_binding graph components u =
  let u_map = Map.find_exn graph u in
  let u_uf = Map.find_exn components u in
  Map.iter_keys u_map ~f:(fun v ->
      let v_uf = Map.find_exn components v in
      Union_find.union u_uf v_uf)

let part3 l =
  let g = to_grid l in
  let graph = find_edges g in
  all_pairs (List.filter ~f:Option.is_some (Map.keys graph))
  |> List.map ~f:(fun (u, v) ->
      let components = to_uf_components graph in
      spell_of_binding graph components u;
      spell_of_binding graph components v;
      sum_weights @@ mst graph components)
  (* |> (fun l ->
  l |> List.sort ~compare |> List.map ~f:Int.to_string |> String.concat ~sep:","
  |> Stdio.printf "%s\n";
  l) *)
  |> List.min_elt ~compare
  |> Option.value_exn

let%test_unit "part3" = [%test_result: int] ~expect:0 (part3 example)
let%test_unit "part3" = [%test_result: int] ~expect:21 (part3 p2_example2)
