[@@@ocaml.warning "-26-32-34-37"]

open Base

type plant_id = int [@@deriving show]

type branch = FreeBranch of int | PlantBranch of (plant_id * int)
[@@deriving show]

type plant = Plant of plant_id * int * branch list [@@deriving show]

let plant_re = Re.Perl.re {|Plant (\d+) with thickness (\d+):|} |> Re.compile

let free_branch_re =
  Re.Perl.re {|\- free branch with thickness (\d+)|} |> Re.compile

let branch_re =
  Re.Perl.re {|\- branch to Plant (\d+) with thickness (-?\d+)|} |> Re.compile

let parse_plant s branches =
  match Re.exec_opt plant_re s with
  | Some groups ->
      Plant
        ( Int.of_string @@ Re.Group.get groups 1,
          Int.of_string @@ Re.Group.get groups 2,
          branches )
  | None -> failwith "invalid input"

let parse_branch s =
  match Re.exec_opt free_branch_re s with
  | Some groups -> FreeBranch (Int.of_string @@ Re.Group.get groups 1)
  | None -> (
      match Re.exec_opt branch_re s with
      | None -> failwith "invalid input"
      | Some groups ->
          PlantBranch
            ( Int.of_string @@ Re.Group.get groups 1,
              Int.of_string @@ Re.Group.get groups 2 ))

let parse_plant l =
  let hd, tl = List.split_n l 1 in
  parse_plant (List.hd_exn hd) (List.map ~f:parse_branch tl)

(* so essentially we need to topological sort here. *)

(* I don't understand why I have to always reverse these lists, feels like
   something I "don't get" *)
let split_list_on l ~splitter =
  let rec loop l section result =
    match l with
    | [] -> List.rev (List.rev section :: result)
    | x :: xs ->
        if splitter x then
          loop xs []
            (if List.length section > 1 then List.rev section :: result
             else result)
        else loop xs (x :: section) result
  in
  loop l [] []

let%test_unit "split_list_on" =
  [%test_eq: int list list]
    [ [ 1; 2; 3 ]; [ 5; 6; 7 ] ]
    (split_list_on [ 1; 2; 3; 4; 5; 6; 7 ] ~splitter:(equal_int 4))

let%test_unit "split_list_on (duplicates)" =
  [%test_eq: int list list]
    [ [ 1; 2; 3 ]; [ 5; 6; 7 ] ]
    (split_list_on [ 1; 2; 3; 4; 4; 5; 6; 7 ] ~splitter:(equal_int 4))

let plant_power plants_by_id is_free_branch_active pid =
  let plant_power = Hashtbl.create (module Int) in
  let rec loop pid =
    match Hashtbl.find plant_power pid with
    | Some p -> p
    | None ->
        let power =
          match Map.find_exn plants_by_id pid with
          | Plant (_, plant_thickness, branches) ->
              let incoming_energy =
                List.fold_right ~init:0
                  ~f:(fun branch energy ->
                    energy
                    +
                    match branch with
                    | FreeBranch _ -> if is_free_branch_active pid then 1 else 0
                    | PlantBranch (branch_pid, thickness) ->
                        loop branch_pid * thickness)
                  branches
              in
              if incoming_energy < plant_thickness then 0 else incoming_energy
        in
        Hashtbl.add_exn plant_power ~key:pid ~data:power;
        power
  in
  loop pid

let part1 l =
  let plant_list =
    l |> split_list_on ~splitter:(equal_string "") |> List.map ~f:parse_plant
  in
  let plants_by_id =
    List.fold_right
      ~f:(fun plant map ->
        match plant with
        | Plant (pid, _, _) -> Map.add_exn map ~key:pid ~data:plant)
      ~init:(Map.empty (module Int))
      plant_list
  in
  match List.last_exn plant_list with
  | Plant (final_pid, _, _) ->
      plant_power plants_by_id (fun _ -> true) final_pid

let part2 l =
  let parsed_list = l |> split_list_on ~splitter:(equal_string "") in
  let plants_list =
    parsed_list
    |> List.sub ~pos:0 ~len:(List.length parsed_list - 1)
    |> List.map ~f:parse_plant
  in
  let plants_by_id =
    List.fold_right
      ~f:(fun plant map ->
        match plant with
        | Plant (pid, _, _) -> Map.add_exn map ~key:pid ~data:plant)
      ~init:(Map.empty (module Int))
      plants_list
  in
  let test_cases = List.last_exn parsed_list in
  match List.last_exn plants_list with
  | Plant (final_pid, _, _) ->
      List.fold_right ~init:0
        ~f:(fun s acc ->
          let branch_activations =
            s |> String.split ~on:' ' |> List.map ~f:Int.of_string
            |> Array.of_list
          in
          acc
          + plant_power plants_by_id
              (fun n -> branch_activations.(n - 1) = 1)
              final_pid)
        test_cases

(* OK I failed to use the linear optimizer for part 3, I got pretty far *)
(* looking at visualization on Reddit it seems like free branches are only ever
   positive or negative *)

let part3 l =
  let parsed_list = l |> split_list_on ~splitter:(equal_string "") in
  let plants_list =
    parsed_list
    |> List.sub ~pos:0 ~len:(List.length parsed_list - 1)
    |> List.map ~f:parse_plant
  in
  let plants_by_id =
    List.fold
      ~f:(fun map plant ->
        match plant with
        | Plant (pid, _, _) -> Map.add_exn map ~key:pid ~data:plant)
      ~init:(Map.empty (module Int))
      plants_list
  in
  let freebranches =
    List.fold
      ~init:(Set.empty (module Int))
      ~f:(fun acc (Plant (pid, _, branches)) ->
        match branches with [ FreeBranch _ ] -> Set.add acc pid | _ -> acc)
      plants_list
  in
  let positive_branches =
    List.fold ~init:freebranches
      ~f:(fun s plant ->
        match plant with
        | Plant (_, _, branches) ->
            List.fold ~init:s
              ~f:(fun freebranches p ->
                match p with
                | PlantBranch (pid, thickness) ->
                    if thickness < 0 then Set.remove freebranches pid
                    else freebranches
                | _ -> freebranches)
              branches)
      plants_list
  in
  let test_cases = List.last_exn parsed_list in
  match List.last_exn plants_list with
  | Plant (final_pid, _, _) ->
      let max_power =
        plant_power plants_by_id
          (fun n -> Set.mem positive_branches n)
          final_pid
      in
      List.fold_right ~init:0
        ~f:(fun s acc ->
          let branch_activations =
            s |> String.split ~on:' ' |> List.map ~f:Int.of_string
            |> Array.of_list
          in
          let power =
            plant_power plants_by_id
              (fun n -> branch_activations.(n - 1) = 1)
              final_pid
          in
          acc + if power = 0 then 0 else max_power - power)
        test_cases
