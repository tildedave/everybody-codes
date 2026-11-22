open Base
open Core
(* open Util *)

let example =
  [
    "x=0 y=5";
    "x=5 y=7";
    "x=6 y=8";
    "x=7 y=1";
    "x=8 y=3";
    "x=10 y=6";
    "x=15 y=3";
    "x=18 y=4";
  ]

let parse_line s = Stdlib.Scanf.sscanf s "x=%d y=%d" (fun x y -> (x, y))
(* let parsed_example = List.map ~f:parse_line example *)

let%test_unit "parse_line" =
  [%test_result: int * int] ~expect:(0, 5) (parse_line "x=0 y=5")

let mould_between (x, y) (x', y') =
  (* assumption: nothing between x, x' *)
  (max x x' - min x x' - 1) * min y y'

let%test_unit "mould_between" =
  [%test_result: int] ~expect:4 (mould_between (2, 3) (5, 2))

(* OK, start at the beginning, walk right, etc *)

let _total_between l start end_ =
  let rec helper curr acc =
    if Doubly_linked.Elt.equal curr end_ then acc
    else
      let _, y = Doubly_linked.Elt.value curr in
      helper (Option.value_exn (Doubly_linked.next l curr)) (acc + y)
  in
  helper (Option.value_exn (Doubly_linked.next l start)) 0

let rec walk l start acc =
  let open Doubly_linked in
  let sx, sy = Elt.value start in
  (* walk right until you find an element >= us - it may not exist *)
  (* if we get to the end, we find the max element seen on the way,
       fill to that (minusing any columns in between), and then recurse from
       that element *)
  let rec walk_right curr max_so_far =
    let x', y' = Elt.value curr in
    if y' >= sy then
      (* this walks a second time, we could keep track of more I suppose,
               but I am lazy *)
      (* Stdio.printf
        "we reached something bigger than us - connecting %s with %s. %d + %d\n"
        (show_tuple (sx, sy))
        (show_tuple (x', y'))
        sy
        (mould_between (sx, sy) (x', y'));
      Stdio.printf "next -> %s\n" (show_tuple (x', y')); *)
      Some (sy + mould_between (sx, sy) (x', y'), curr)
    else
      let next_max =
        match max_so_far with
        | None -> Some curr
        | Some elt ->
            let _, ym = Elt.value elt in
            if y' >= ym then Some curr else Some elt
      in
      match next l curr with
      | None -> (
          (* we are at the end of the list.  we fill between the start
                   and the max seen so far, then go to the max seen so far *)
          match next_max with
          | None -> None (* at the end of the list, never saw anything else *)
          | Some e ->
              let xm, ym = Elt.value e in
              (* Stdio.printf
                "we reached the end of the list - connecting %s with %s. %d + %d\n"
                (show_tuple (sx, sy))
                (show_tuple (xm, ym))
                sy
                (mould_between (sx, sy) (xm, ym));
              Stdio.printf "next -> %s\n" (show_tuple (xm, ym)); *)
              Some (sy + mould_between (sx, sy) (xm, ym), e))
      | Some e -> walk_right e next_max
  in
  match next l start with
  | None -> acc + sy
  | Some elt -> (
      match walk_right elt None with
      | None ->
          failwith "I think this is impossible "
          (* (acc + y)  (* not clear how this could happen *) *)
      | Some (n, next) -> walk l next (acc + n))

(* walk left until you find an element >= us - it may not exist *)
(* if we get to the end, then it fills to the max number of columns seem *)

(*let rec matching_tower ((_, y) as curr) l acc =
  match l with
  | [] -> None
  | (x', y') :: xs -> if y' >= y then Some ((x', y'), acc) else matching_tower curr xs (acc + y')

let%test_unit "matching_tower" =
  [%test_result: ((int * int) * int) option] ~expect:(Some ((5, 7), 0))
    (matching_tower (0, 5) (List.tl_exn parsed_example) 0)

let%test_unit "matching_tower (None)" =
  [%test_result: ((int * int) * int) option] ~expect:None
    (matching_tower (10, 6) (List.drop parsed_example 6) 0)

let%test_unit "matching_tower (None)" =
  [%test_result: ((int * int) * int) option] ~expect:None
    (matching_tower (10, 6) (List.drop parsed_example 6) 0)
    (* let%test_unit "next matcher" =
    [%test_result: int] *)

let part1 l =
    let sorted_towers =
  l |> List.map ~f:parse_line
  |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst) in
  |> (let rec helper l acc =
    match l with
    | [] -> acc
    | (x, y) :: xs ->
        match matching_tower (x, y) with
        | None ->  helper xs (acc + y)
        | Some
        )
  |> Sequence.of_list
  |> (fun s -> Sequence.zip s (Sequence.tl_eagerly_exn s))
  |> Sequence.sum
       (module Int)
       ~f:(fun (((_, y) as a), b) ->
         Stdio.printf "%s <--> %s\n" (show_tuple a) (show_tuple b);
         y + mould_between a b) *)

let part1 l =
  let ll = l |> List.map ~f:parse_line |> Doubly_linked.of_list in
  walk ll (Option.value_exn @@ Doubly_linked.first_elt ll) 0

let%test_unit "part1" = [%test_result: int] ~expect:96 (part1 example)

(* we can fake part2 pretty well.  presumably faking will not work for part3 *)

let p2_parse s =
  Stdlib.Scanf.sscanf s "x=%d y=%d width=%d" (fun x y width ->
      List.init width ~f:(fun dx -> (x + dx, y)))

let part2 l =
  let ll = l |> List.concat_map ~f:p2_parse |> Doubly_linked.of_list in
  walk ll (Option.value_exn @@ Doubly_linked.first_elt ll) 0

let p2_example =
  [
    "x=0 y=5 width=1";
    "x=1 y=4 width=3";
    "x=5 y=3 width=2";
    "x=7 y=9 width=1";
    "x=8 y=8 width=1";
    "x=9 y=9 width=3";
    "x=12 y=3 width=1";
    "x=16 y=7 width=1";
    "x=19 y=3 width=5";
    "x=25 y=5 width=1";
    "x=29 y=1 width=2";
    "x=31 y=6 width=1";
  ]

let%test_unit "part2" = [%test_result: int] ~expect:205 (part2 p2_example)
let part3 _ = 1
