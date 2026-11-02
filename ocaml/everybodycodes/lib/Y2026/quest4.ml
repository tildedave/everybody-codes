open Base

let line_re =
  Re.Perl.re {|id=(\d+) quest=(\d+) part=(\d+) time=(\d+)|} |> Re.compile

type info = { id : int; (* quest : int; part : int;  *) time : int }

let parse_line s =
  match Re.exec_opt line_re s with
  | Some groups ->
      {
        id = Int.of_string @@ Re.Group.get groups 1;
        (* quest = Int.of_string @@ Re.Group.get groups 2;
        part = Int.of_string @@ Re.Group.get groups 3;*)
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

let part1 l = l |> parse_lines |> final_result
let part2 _ = 1
let part3 _ = 1
