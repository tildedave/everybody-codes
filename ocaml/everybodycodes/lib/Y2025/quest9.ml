open Base
open Util
open Core

let is_child candidate others =
  with_return (fun r ->
      List.iter
        ~f:(fun n ->
          let candidate_ch = String.get candidate n in
          if
            Set.for_all
              ~f:(fun other ->
                not (equal_char candidate_ch (String.get other n)))
              others
          then r.return false)
        (0 -- (String.length candidate - 1));
      true)

let part1_given1 = "CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG"
let part1_given2 = "CTTGAATTGGGCCGTTTACCTGGTTTAACCAT"
let part1_given3 = "CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG"

let similarity_score s1 s2 =
  0 -- (String.length s1 - 1)
  |> List.filter ~f:(fun n -> equal_char (String.get s1 n) (String.get s2 n))
  |> List.length

let%test_unit "is_child given (1)" =
  [%test_eq: bool] false
    (is_child part1_given1
    @@ set_add_all (Set.empty (module String)) [ part1_given2; part1_given3 ])

let%test_unit "is_child given (2)" =
  [%test_eq: bool] false
    (is_child part1_given2
    @@ set_add_all (Set.empty (module String)) [ part1_given1; part1_given3 ])

let%test_unit "is_child given (3)" =
  [%test_eq: bool] true
    (is_child part1_given3
    @@ set_add_all (Set.empty (module String)) [ part1_given1; part1_given2 ])

let%test_unit "similarity score (1)" =
  [%test_eq: int] 23 (similarity_score part1_given3 part1_given1)

let%test_unit "similarity score (2)" =
  [%test_eq: int] 18 (similarity_score part1_given3 part1_given2)

let all_sequences l =
  set_add_all
    (Set.empty (module String))
    (List.map ~f:(fun s -> List.nth_exn (String.split ~on:':' s) 1) l)

let scale_mapping l =
  List.fold
    ~init:(Map.empty (module String))
    ~f:(fun acc s ->
      match String.split ~on:':' s with
      | [ id_str; l' ] -> Map.add_exn acc ~key:l' ~data:(Int.of_string id_str)
      | _ -> failwith "bad input")
    l

let part1 l =
  let seqs = all_sequences l in
  let child =
    seqs
    |> Set.filter ~f:(fun s -> is_child s (Set.remove seqs s))
    |> Set.find ~f:(fun _ -> true)
    |> Option.value_exn
  in
  Set.remove seqs child
  |> Set.fold ~f:(fun acc s -> acc * similarity_score child s) ~init:1

let%test_unit "part1 (given)" =
  [%test_eq: int] 414
    (part1
       [
         "1:CAAGCGCTAAGTTCGCTGGATGTGTGCCCGCG";
         "2:CTTGAATTGGGCCGTTTACCTGGTTTAACCAT";
         "3:CTAGCGCTGAGCTGGCTGCCTGGTTGACCGCG";
       ])

let find_parents s all_pairs =
  with_return (fun r ->
      all_pairs
      |> List.filter ~f:(fun (s1, s2) ->
             (not (equal_string s1 s))
             && (not (equal_string s2 s))
             && equal_int (compare_string s1 s2) (-1))
      |> List.iter ~f:(fun (p1, p2) ->
             if is_child s (set_add_all (Set.empty (module String)) [ p1; p2 ])
             then r.return (Some (p1, p2)));
      None)

(* we will just brute force part 2 I suppose *)
let part2 l =
  let seqs = all_sequences l in
  let all_pairs =
    let _set_list = Set.to_list seqs in
    List.cartesian_product _set_list _set_list
  in
  seqs
  |> Set.fold ~init:0 ~f:(fun acc s ->
         match find_parents s all_pairs with
         | None -> acc
         | Some (s1, s2) -> acc + (similarity_score s s1 * similarity_score s s2))

let%test_unit "part2 (given)" =
  [%test_eq: int] 1245
    (part2
       [
         "1:GCAGGCGAGTATGATACCCGGCTAGCCACCCC";
         "2:TCTCGCGAGGATATTACTGGGCCAGACCCCCC";
         "3:GGTGGAACATTCGAAAGTTGCATAGGGTGGTG";
         "4:GCTCGCGAGTATATTACCGAACCAGCCCCTCA";
         "5:GCAGCTTAGTATGACCGCCAAATCGCGACTCA";
         "6:AGTGGAACCTTGGATAGTCTCATATAGCGGCA";
         "7:GGCGTAATAATCGGATGCTGCAGAGGCTGCTG";
       ])

let part3 l =
  let seqs = all_sequences l in
  let all_pairs =
    let _set_list = Set.to_list seqs in
    List.cartesian_product _set_list _set_list
  in
  let set_mapping =
    Set.fold
      ~init:(Map.empty (module String))
      ~f:(fun acc s -> Map.add_exn acc ~key:s ~data:(Union_find.create s))
      seqs
  in
  Set.iter
    ~f:(fun s ->
      match find_parents s all_pairs with
      | None -> ()
      | Some (s1, s2) ->
          Union_find.union
            (Map.find_exn set_mapping s)
            (Map.find_exn set_mapping s1);
          Union_find.union
            (Map.find_exn set_mapping s1)
            (Map.find_exn set_mapping s2))
    seqs;
  let winner_group =
    set_mapping
    |> Map.fold
         ~init:(Map.empty (module String))
         ~f:(fun ~key:_ ~data acc ->
           Map.update acc
             ~f:(fun v -> match v with None -> 1 | Some n -> n + 1)
             (Union_find.get data))
    |> Map.fold
         ~f:(fun ~key ~data (winner, max_so_far) ->
           if data > max_so_far then (key, data) else (winner, max_so_far))
         ~init:("", 0)
    |> fst
  in
  let mapping = scale_mapping l in
  Map.fold ~init:0
    ~f:(fun ~key ~data acc ->
      if equal_string (Union_find.get data) winner_group then
        acc + Map.find_exn mapping key
      else acc)
    set_mapping

(* find where in our current forest this one goes
   I guess in full generality this requires a disjoint set data structure bleh *)
let%test_unit "part3 (given)" =
  [%test_eq: int] 12
    (part3
       [
         "1:GCAGGCGAGTATGATACCCGGCTAGCCACCCC";
         "2:TCTCGCGAGGATATTACTGGGCCAGACCCCCC";
         "3:GGTGGAACATTCGAAAGTTGCATAGGGTGGTG";
         "4:GCTCGCGAGTATATTACCGAACCAGCCCCTCA";
         "5:GCAGCTTAGTATGACCGCCAAATCGCGACTCA";
         "6:AGTGGAACCTTGGATAGTCTCATATAGCGGCA";
         "7:GGCGTAATAATCGGATGCTGCAGAGGCTGCTG";
       ])

let%test_unit "part3 (given; 2)" =
  [%test_eq: int] 36
    (part3
       [
         "1:GCAGGCGAGTATGATACCCGGCTAGCCACCCC";
         "2:TCTCGCGAGGATATTACTGGGCCAGACCCCCC";
         "3:GGTGGAACATTCGAAAGTTGCATAGGGTGGTG";
         "4:GCTCGCGAGTATATTACCGAACCAGCCCCTCA";
         "5:GCAGCTTAGTATGACCGCCAAATCGCGACTCA";
         "6:AGTGGAACCTTGGATAGTCTCATATAGCGGCA";
         "7:GGCGTAATAATCGGATGCTGCAGAGGCTGCTG";
         "8:GGCGTAAAGTATGGATGCTGGCTAGGCACCCG";
       ])
