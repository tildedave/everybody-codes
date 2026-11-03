open Base

let line_re =
  Re.Perl.re {|id=(\d+) quest=(\d+) part=(\d+) time=(\d+)|} |> Re.compile

type info = { id : int; quest : int; part : int; time : int }

let parse_line s =
  match Re.exec_opt line_re s with
  | Some groups ->
      {
        id = Int.of_string @@ Re.Group.get groups 1;
        quest = Int.of_string @@ Re.Group.get groups 2;
        part = Int.of_string @@ Re.Group.get groups 3;
        time = Int.of_string @@ Re.Group.get groups 4;
      }
  | None -> failwith "invalid input"

let parse_lines l =
  List.fold
    ~init:(Map.empty (module Int))
    ~f:(fun acc s ->
      let r = parse_line s in
      Map.update acc r.id ~f:(fun v ->
          match v with None -> [ r ] | Some l -> r :: l))
    l

let total_time m x =
  Map.find_exn m x |> List.fold ~init:0 ~f:(fun acc i -> acc + i.time)

let num_parts m x = List.length (Map.find_exn m x)

let compare_participants m x y =
  match compare (num_parts m x) (num_parts m y) with
  | 0 -> compare (total_time m x) (total_time m y)
  | x -> -x

let final_result m =
  List.sort (Map.keys m) ~compare:(compare_participants m)
  |> List.map ~f:(fun x -> total_time m x)
  |> List.foldi ~init:0 ~f:(fun n acc t -> ((n + 1) * t) + acc)

let%test_unit "final_result (pt1)" =
  [%test_eq: int] 27598
    (final_result
       (parse_lines
          [
            "id=1 quest=1 part=1 time=187";
            "id=3 quest=1 part=1 time=413";
            "id=5 quest=1 part=3 time=744";
            "id=2 quest=1 part=3 time=745";
            "id=4 quest=1 part=2 time=558";
            "id=1 quest=1 part=2 time=299";
            "id=1 quest=1 part=3 time=362";
            "id=4 quest=1 part=3 time=1268";
            "id=4 quest=1 part=1 time=457";
            "id=5 quest=1 part=1 time=257";
            "id=2 quest=1 part=1 time=192";
            "id=3 quest=1 part=3 time=1168";
            "id=5 quest=1 part=2 time=346";
            "id=3 quest=1 part=2 time=455";
            "id=2 quest=1 part=2 time=562";
          ]))

let%test_unit "final_result (pt2)" =
  [%test_eq: int] 6470
    (final_result
       (parse_lines
          [
            "id=2 quest=1 part=1 time=100";
            "id=4 quest=2 part=1 time=150";
            "id=1 quest=2 part=1 time=140";
            "id=1 quest=1 part=2 time=180";
            "id=2 quest=2 part=1 time=130";
            "id=4 quest=1 part=1 time=110";
            "id=1 quest=2 part=2 time=210";
            "id=2 quest=2 part=2 time=240";
            "id=1 quest=2 part=3 time=330";
            "id=2 quest=1 part=2 time=170";
            "id=3 quest=1 part=1 time=90";
            "id=4 quest=1 part=2 time=175";
            "id=3 quest=2 part=1 time=120";
            "id=1 quest=1 part=1 time=120";
            "id=3 quest=2 part=2 time=220";
            "id=2 quest=2 part=3 time=350";
            "id=3 quest=1 part=2 time=160";
          ]))

(*
    part3 seems fine.  will need to organize my logic a bit better I think
    go through each quest, 1 at a time

    -- the same combined time after a given part -- implies part1 + part2 matters etc

    for each round, we calculate score for a player.
    part2 needs to look at part1 times
    part3 needs to look at part1 and part2 times
*)

let all_players = Map.keys

let all_quests m =
  m
  |> Map.fold
       ~init:(Set.empty (module Int))
       ~f:(fun ~key:_ ~data acc ->
         List.fold ~init:acc ~f:(fun acc i -> Set.add acc i.quest) data)
  |> Set.to_list |> List.sort ~compare

let has_completed m i quest part =
  Map.find_exn m i
  |> List.filter ~f:(fun i -> equal i.quest quest && equal i.part part)
  |> List.is_empty |> not

let total_time m quest part player =
  Map.find_exn m player
  |> List.filter ~f:(fun i -> equal i.quest quest && i.part <= part)
  |> List.fold ~init:0 ~f:(fun acc i -> i.time + acc)

let rank m quest part =
  all_players m
  |> List.filter ~f:(fun i -> has_completed m i quest part)
  |> List.map ~f:(fun i -> (i, total_time m quest part i))
  |> List.sort ~compare:(fun (_, t1) (_, t2) -> compare t1 t2)

let points m ~quest ~part =
  let ranked = rank m quest part in
  match List.hd ranked with
  | None -> []
  | Some winner -> (
      let starting_points =
        match part with
        | 1 -> 50
        | 2 -> 100
        | 3 -> 150
        | _ -> failwith "invalid part"
      in
      match
        List.fold
          ~init:
            (snd winner, starting_points, 0, [ (fst winner, starting_points) ])
          ~f:(fun (prev_time, next_score, num_tied, l) (i, i_time) ->
            if equal prev_time i_time then
              (i_time, next_score, num_tied + 1, (i, next_score) :: l)
            else
              let score = max (next_score - num_tied - 1) 0 in
              (i_time, score, 0, (i, score) :: l))
          (List.tl_exn ranked)
      with
      | _, _, _, l -> List.rev l)

let tie_test =
  [
    "id=2 quest=1 part=1 time=90";
    "id=4 quest=2 part=1 time=150";
    "id=1 quest=2 part=1 time=130";
    "id=1 quest=1 part=2 time=180";
    "id=2 quest=2 part=1 time=130";
    "id=4 quest=1 part=1 time=110";
    "id=1 quest=2 part=2 time=210";
    "id=2 quest=2 part=2 time=220";
    "id=1 quest=2 part=3 time=330";
    "id=2 quest=1 part=2 time=170";
    "id=3 quest=1 part=1 time=90";
    "id=4 quest=1 part=2 time=170";
    "id=3 quest=2 part=1 time=120";
    "id=1 quest=1 part=1 time=120";
    "id=3 quest=2 part=2 time=220";
    "id=2 quest=2 part=3 time=350";
    "id=3 quest=1 part=2 time=170";
  ]

let%test_unit "points (quest1 pt1)" =
  [%test_eq: (int * int) list]
    [ (2, 50); (3, 50); (4, 48); (1, 47) ]
    (points (parse_lines tie_test) ~quest:1 ~part:1)

let%test_unit "points (quest1 pt2)" =
  [%test_eq: (int * int) list]
    [ (2, 100); (3, 100); (4, 98); (1, 97) ]
    (points (parse_lines tie_test) ~quest:1 ~part:2)

let%test_unit "points (quest1 pt3)" =
  [%test_eq: (int * int) list] []
    (points (parse_lines tie_test) ~quest:1 ~part:3)

let%test_unit "points (quest2 pt1)" =
  [%test_eq: (int * int) list]
    [ (3, 50); (1, 49); (2, 49); (4, 47) ]
    (points (parse_lines tie_test) ~quest:2 ~part:1)

let%test_unit "points (quest2 pt2)" =
  [%test_eq: (int * int) list]
    [ (1, 100); (3, 100); (2, 98) ]
    (points (parse_lines tie_test) ~quest:2 ~part:2)

let%test_unit "points (quest2 pt3)" =
  [%test_eq: (int * int) list]
    [ (1, 150); (2, 149) ]
    (points (parse_lines tie_test) ~quest:2 ~part:3)

(* OK we should be fine to complete this *)

let final_ranking m =
  let parts = List.init 3 ~f:(fun n -> n + 1) in
  all_quests m
  |> List.concat_map ~f:(fun quest ->
      List.concat_map parts ~f:(fun part -> points m ~quest ~part))
  |> Map.of_alist_reduce (module Int) ~f:( + )
  |> Map.to_alist
  |> List.sort ~compare:(fun (_, pt1) (_, pt2) -> compare pt2 pt1)
(* apparently can Comparator.lift the last bit *)

let%test_unit "final ranking" =
  [%test_eq: (int * int) list]
    [ (2, 446); (1, 443); (3, 300); (4, 193) ]
    (final_ranking (parse_lines tie_test))

let checksum l =
  let _, winning_pts = List.hd_exn l in
  let _, _, _, result =
    List.fold
      ~init:(1, winning_pts, 0, winning_pts)
      ~f:(fun (place, last_pts, num_tied, total) (_, pts) ->
        if equal last_pts pts then
          (place, pts, num_tied + 1, total + (place * pts))
        else
          let next_place = place + num_tied + 1 in
          (next_place, pts, 0, total + (next_place * pts)))
      (List.tl_exn l)
  in
  result

let%test_unit "checksum" =
  [%test_eq: int] 3004 (checksum (final_ranking (parse_lines tie_test)))

let%test_unit "checksum (tie)" =
  [%test_eq: int] (50 + 50 + (3 * 49)) (checksum [ (1, 50); (2, 50); (3, 49) ])

let part1 l = l |> parse_lines |> final_result
let part2 = part1
let part3 l = l |> parse_lines |> final_ranking |> checksum
