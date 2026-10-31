open Base
open Util

let parse_pair s =
  match
    List.map ~f:Int.of_string @@ String.split_on_chars ~on:[ '<'; '>' ] s
  with
  | [ first; second ] ->
      if String.mem s '<' then (first, second) else (second, first)
  | _ -> failwith "invalid input"

let%test_unit "parse_pair (<)" = [%test_eq: int * int] (4, 5) (parse_pair "4<5")
let%test_unit "parse_pair (>)" = [%test_eq: int * int] (5, 4) (parse_pair "4>5")

(* we'll start with a silly way.  I suppose it will be good enough. *)

let parse_alloys l =
  let parsed = l |> String.split ~on:' ' |> List.map ~f:parse_pair in
  let order =
    parsed |> List.fold ~init:(Set.empty (module IntPair_Comparator)) ~f:Set.add
  in
  let alloys =
    List.fold
      ~init:(Set.empty (module Int))
      ~f:(fun acc (x, y) -> Set.add (Set.add acc x) y)
      parsed
  in
  (Set.to_list alloys, order)

(* approach will be DFS *)

let rec depth_first_search order (visited, answer) node =
  let final_visited, final_answer =
    Set.fold
      ~init:(Set.add visited node, answer)
      ~f:(fun (visited, answer) (_, neighbor) ->
        if not (Set.mem visited neighbor) then
          depth_first_search order (visited, answer) neighbor
        else (visited, answer))
      (Set.filter order ~f:(fun (x, _) -> equal x node))
  in
  (final_visited, node :: final_answer)

let toposort alloys order =
  alloys
  |> List.fold
       ~f:(fun (visited, answer) node ->
         if not (Set.mem visited node) then
           depth_first_search order (visited, answer) node
         else (visited, answer))
       ~init:(Set.empty (module Int), [])
  |> snd

let alloy_order l =
  let alloys, order = parse_alloys l in
  toposort alloys order |> List.rev |> List.map ~f:Int.to_string
  |> String.concat |> Int.of_string

let%test_unit "alloy order" =
  [%test_eq: int] 35124 (alloy_order "4<3 2<5 3>1 1>2 1<5 4<1 3>2 4<2 3>5 4<5")

let%test_unit "alloy order 2" =
  [%test_eq: int] 43512 (alloy_order "5>1 5>2 4>1 3>2 3<4 1>2 3>5 1<3 4>5")

let part1 lines = List.fold ~f:( + ) ~init:0 @@ List.map ~f:alloy_order lines

let%test_unit "part1" =
  [%test_eq: int] 122157
    (part1
       [
         "4<3 2<5 3>1 1>2 1<5 4<1 3>2 4<2 3>5 4<5";
         "4>1 2>1 3>2 1<5 4>5 2<5 3>1 4>3 2<4 5<3";
         "5>1 5>2 4>1 2<4 3>2 3<4 1>2 3>5 1<3 4>5";
       ])

(* part2 and part1 are the same really, the input just keeps narrowing *)

let part2 = part1
let part3 = part1
