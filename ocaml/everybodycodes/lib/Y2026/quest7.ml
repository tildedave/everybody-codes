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

(* it's all lanternfish?  always has been *)
let signal connections_m ?(explode_at = None) (impulses, energy_m) =
  if Map.is_empty impulses then None
  else
    let next_state =
      Map.fold impulses
        ~init:(Map.empty (module Int), energy_m)
        ~f:(fun ~key:node ~data:(energy, count) (impulses, energy_m) ->
          let energy_m = add_energy energy_m node (energy * count) in
          match energy with
          | 1 -> (impulses, energy_m)
          | _ ->
              (* update the impulses to add the count *)
              ( List.fold ~init:impulses
                  ~f:(fun acc neighbor ->
                    Map.update acc neighbor ~f:(fun v ->
                        match v with
                        | None -> [(energy - 1, count)]
                        | Some (_, vl) -> (energy - 1, v + count)))
                    (* energy - 1 is always the same *)
                  (Map.find_exn connections_m node),
                energy_m ))
    in
    Option.value_map explode_at
      ~default:(Some (snd next_state, next_state))
      ~f:(fun n_explosion ->
        let impulses, energy_m = next_state in
        (* could ~f:(<= n_explosion )*)
        let explosions =
          Map.keys (Map.filter energy_m ~f:(fun n -> n >= n_explosion))
        in
        (* explosions propagate at energy 10 to all connections *)
        (* must first prune exploding keys *)
        List.fold
        ~init:(List.fold ~init:impulses ~f:Map.remove)
        ~f:(fun acc node ->
            List.fold
            ~init:acc
            ~f:(fun v ->
                match v with
                | None -> (10, 1))
                | Some (_, v) -> ()
            (Map.find_exn connections_m node)
            )

(* id = 1, energy = 5, connections = 2, 5, 7 *)
(* let parse_line s = *)

let propagate initial l =
  let energy_m, connections_m = parse_lines l in
  Sequence.unfold
    ~init:
      ( Map.singleton (module Int) (starting_cloud connections_m) (initial, [1]),
        energy_m )
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

let part3 _ = 1
