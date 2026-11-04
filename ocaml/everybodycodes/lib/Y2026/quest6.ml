open Base

let parse_line s =
  match String.split ~on:':' s with
  | [ source; rest ] -> (source, String.split ~on:',' rest)
  | _ -> failwith "invalid"

let parse_lines l = Map.of_alist_exn (module String) (List.map ~f:parse_line l)

(* then we just dfs, again this is topo sort *)

let rec dfs m visited node =
  List.fold ~init:(Set.add visited node)
    ~f:(fun visited neighbor ->
      if not (Set.mem visited neighbor) then dfs m visited neighbor else visited)
    (Option.value (Map.find m node) ~default:[])

let example =
  [
    "BRDS:FLWR,SLTT,YSTS";
    "FLWR:SLTT,YSTS";
    "SLTT:YSTS,MLKK,BTTR";
    "NULL:BRDS,FLWR,SLTT";
    "YSTS:BRDS,FLWR";
    "MLKK:SPCS,NULL";
    "HNYN:BRDS,FLWR,SLTT,YSTS";
  ]

let part1 l =
  (Set.length @@ dfs (parse_lines l) (Set.empty (module String)) "NULL") - 1

let%test_unit "part1" = [%test_eq: int] 7 (part1 example)

(* part2 is part1 but we reverse the graph direction *)

let reverse_map m =
  List.fold
    ~init:(Map.empty (module String))
    ~f:(fun acc (from, to_list) ->
      List.fold ~init:acc
        ~f:(fun acc to_ ->
          Map.update acc to_ ~f:(fun v ->
              match v with None -> [ from ] | Some l -> from :: l))
        to_list)
    (Map.to_alist m)

let part2 l =
  (Set.length
  @@ dfs (reverse_map (parse_lines l)) (Set.empty (module String)) "NULL")
  - 1

let%test_unit "part2" = [%test_eq: int] 6 (part2 example)

(* this is simple *)
let part3 l =
  let m = reverse_map (parse_lines l) in
  let v1 = dfs m (Set.empty (module String)) "NULLv1" in
  let v2 = dfs m (Set.empty (module String)) "NULLv2" in
  let v3 = dfs m (Set.empty (module String)) "NULLv3" in
  Set.length
  @@ List.fold ~init:(Set.inter v1 v2) ~f:Set.union
       [ Set.inter v2 v3; Set.inter v1 v3 ]

let example_pt3 =
  [
    "MEAL:BRDS,CAKE,DOUG";
    "BRDS:DOUG,FLWR";
    "CAKE:DOUG,FLWR,NULLv2,NULLv3";
    "DOUG:FLWR,YSTS,SLTT,NULLv1";
    "FLWR:BRDS,DOUG";
    "YSTS:SPCS";
    "SOUP:BRDS,CAKE,DOUG,FLWR";
  ]

let%test_unit "part3" = [%test_eq: int] 3 (part3 example_pt3)
