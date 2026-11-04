open Base

(* let line_re =
  Re.Perl.re {|id=(\d+), energy=(\d+), connections=([\d+,]+)|} |> Re.compile *)

let parse_line s =
  Stdlib.Scanf.sscanf s "id=%d, energy=%d, connections=%s"
    (fun id energy connections ->
      ( (id, energy),
        (id, List.map ~f:Int.of_string @@ String.split ~on:',' connections) ))

let parse_lines l =
  let energy, connections = List.unzip @@ List.map ~f:parse_line l in
  ( Map.of_alist_exn (module Int) energy,
    Map.of_alist_exn (module Int) connections )

let starting_cloud connections_m =
  Map.to_sequence connections_m
  |> Sequence.max_elt
       ~compare:(Comparable.lift Int.compare ~f:(fun (_, k) -> List.length k))
  |> Option.value_exn |> fst

let example =
  [
    "id=1, energy=5, connections=2,5,7";
    "id=2, energy=9, connections=6,7";
    "id=3, energy=3, connections=4";
    "id=4, energy=8, connections=3";
    "id=5, energy=1, connections=6,7";
    "id=6, energy=7, connections=1,7";
    "id=7, energy=6, connections=5";
  ]

let%test_unit "starting_cloud" =
  [%test_eq: int] 1 (starting_cloud (snd (parse_lines example)))

let add_energy energy_m node e =
  Map.update energy_m node ~f:(fun v ->
      match v with None -> failwith "invalid" | Some n -> n + e)
(*
      fun acc neighbor ->
        acc neighbor Map.update acc neighbor ~f:(add_impulse (energy - 1) count) *)

let add_impulse exploded ~energy:e ~count:cnt m node =
  if Set.mem exploded node then m
  else
    Map.update m node ~f:(fun v ->
        match v with
        | None -> Map.singleton (module Int) e cnt
        | Some m' ->
            Map.update m' e ~f:(fun v ->
                match v with None -> cnt | Some cnt' -> cnt + cnt'))

(* it's all lanternfish?  always has been *)
(* impulses are now a map from node -> map of energy -> count, bleh *)
let signal connections_m ?(explode_at = None) (impulses, energy_m, exploded) =
  if Map.is_empty impulses then None
  else
    (* assumption: exploded is constant in pt1 *)
    let next_state =
      Map.fold impulses
        ~init:(Map.empty (module Int), energy_m)
        ~f:(fun ~key:node ~data:node_impulses (impulses, energy_m) ->
          Map.fold node_impulses ~init:(impulses, energy_m)
            ~f:(fun ~key:energy ~data:count (impulses, energy_m) ->
              let energy_m = add_energy energy_m node (energy * count) in
              match energy with
              | 1 -> (impulses, energy_m)
              | _ ->
                  (* update the impulses to add the count *)
                  ( List.fold ~init:impulses
                      ~f:(add_impulse exploded ~energy:(energy - 1) ~count)
                      (Map.find_exn connections_m node),
                    energy_m )))
    in
    Option.value_map explode_at
      ~default:
        (Some (snd next_state, (fst next_state, snd next_state, exploded)))
      ~f:(fun n_explosion ->
        let impulses, energy_m = next_state in
        (* could ~f:(<= n_explosion )*)
        let new_explosions =
          Map.keys (Map.filter energy_m ~f:(fun n -> n >= n_explosion))
        in
        let next_impulses =
          List.fold ~init:impulses ~f:Map.remove new_explosions
        in
        let next_exploded =
          Set.union exploded (Set.of_list (module Int) new_explosions)
        in
        let next_energy =
          List.fold ~init:energy_m ~f:Map.remove new_explosions
        in
        (* explosions propagate at energy 10 to all connections *)
        (* must first prune exploding keys *)
        Some
          ( next_energy,
            ( List.fold ~init:next_impulses
                ~f:(fun acc n ->
                  List.fold ~init:acc
                    ~f:(add_impulse next_exploded ~energy:10 ~count:1)
                    (Map.find_exn connections_m n))
                new_explosions,
              next_energy,
              next_exploded ) ))

(* id = 1, energy = 5, connections = 2, 5, 7 *)
(* let parse_line s = *)

let propagate initial l =
  let energy_m, connections_m = parse_lines l in
  Sequence.unfold
    ~init:
      ( Map.singleton
          (module Int)
          (starting_cloud connections_m)
          (Map.singleton (module Int) initial 1),
        energy_m,
        Set.empty (module Int) )
    ~f:(signal connections_m)
  |> Sequence.to_list_rev |> List.hd_exn |> Map.data
  |> List.sum (module Int) ~f:Fn.id

let part1 = propagate 5
let%test_unit "part1" = [%test_eq: int] 102 (part1 example)
let part2 = propagate 10
let%test_unit "part2" = [%test_eq: int] 1316 (part2 example)

let%test_unit "part2 (another one)" =
  [%test_eq: int] 1214633
    (part2
       [
         "id=1, energy=5, connections=2,3,5,7";
         "id=2, energy=9, connections=1,3,4,5,6,7";
         "id=3, energy=3, connections=1,2,4,6,7";
         "id=4, energy=8, connections=1,2,7";
         "id=5, energy=1, connections=2,3,4,6";
         "id=6, energy=7, connections=1,2,3,4";
         "id=7, energy=6, connections=1,2,4,6";
       ])

let maximize initial l ~explode_at =
  let energy_m, connections_m = parse_lines l in
  let all_clouds = Map.keys connections_m in
  all_clouds
  |> List.map ~f:(fun c ->
      let energy =
        Sequence.unfold
          ~init:
            ( Map.singleton (module Int) c (Map.singleton (module Int) initial 1),
              energy_m,
              Set.empty (module Int) )
          ~f:(signal connections_m ~explode_at)
        |> Sequence.to_list_rev |> List.hd_exn |> Map.data
        |> List.sum (module Int) ~f:Fn.id
      in
      (c, energy))
  |> List.max_elt ~compare:(Comparable.lift Int.descending ~f:snd)
  |> Option.value_exn |> snd

let part3 _ = 1

let%test_unit "maximize" =
  [%test_eq: int] 187 (maximize 10 example ~explode_at:(Some 100))
