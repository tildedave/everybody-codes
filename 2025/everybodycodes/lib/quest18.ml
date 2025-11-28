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

(* time to bust out the linear optimizer for part 3 :-) *)
(* OK so issue with linear programming is that if the incoming energy < thickness -> plant energy is 0 *)
(* can handle this with variables representing the plant being "on" *)

(* only "gotcha" is the thickness *)
(* p_outgoing = if sum p_incoming > thickness then sum p_incoming else 0 *)
(* constraint outgoing energy == 0 OR outgoing_energy = inc*)
(* plant_on * incoming energy >= thickness *)

(* OK, something like this will work *)

let large_c = Lp.c 1000000.0

let build plants_by_id last_pid =
  let open Lp in
  let p_inc = Hashtbl.create (module Int) in
  let p_out = Hashtbl.create (module Int) in
  let y_pon = Hashtbl.create (module Int) in
  let y_freebranches = Hashtbl.create (module Int) in
  Map.iter_keys plants_by_id ~f:(fun pid ->
      Hashtbl.add_exn p_inc ~key:pid ~data:(var (Printf.sprintf "p%d_inc" pid));
      Hashtbl.add_exn p_out ~key:pid ~data:(var (Printf.sprintf "p%d_out" pid));
      Hashtbl.add_exn y_pon ~key:pid
        ~data:(var (Printf.sprintf "yp%d_on" pid) ~integer:true ~lb:0.0 ~ub:1.0);
      match Map.find_exn plants_by_id pid with
      | Plant (_, _, branches) ->
          List.iter branches ~f:(fun branch ->
              match branch with
              | FreeBranch _ ->
                  Hashtbl.add_exn y_freebranches ~key:pid
                    ~data:
                      (var
                         (Printf.sprintf "free%d" pid)
                         ~integer:true ~lb:0. ~ub:1.)
              | _ -> ()));
  let z = var "z" in
  let constraints =
    Map.fold plants_by_id
      ~init:[ eq z (Hashtbl.find_exn p_out last_pid) ]
      ~f:(fun ~key ~data acc ->
        match data with
        | Plant (_, plant_thickness, branches) ->
            let incoming_energy =
              List.fold ~f:( ++ ) ~init:(c 0.0)
                (List.map
                   ~f:(fun branch ->
                     match branch with
                     | FreeBranch _ -> Hashtbl.find_exn y_freebranches key
                     | PlantBranch (branch_id, branch_thickness) ->
                         Hashtbl.find_exn p_out branch_id
                         *~ c (Float.of_int branch_thickness))
                   branches)
            in
            let b, y_in, y_out =
              ( Hashtbl.find_exn y_pon key,
                Hashtbl.find_exn p_inc key,
                Hashtbl.find_exn p_out key )
            in
            eq incoming_energy y_in
            :: (y_in <~ c (Float.of_int plant_thickness) ++ (large_c *~ b))
            :: (y_in
               >~ c (Float.of_int plant_thickness) -- (large_c *~ (c 1. -- b)))
            :: (y_out <~ y_in ++ (large_c *~ (c 1. -- b)))
            :: (y_out >~ y_in -- (large_c *~ (c 1. -- b)))
            :: (y_out <~ large_c *~ b)
            :: (y_out >~ c 0.0)
            :: acc)
  in
  Stdio.printf "%s\n"
    (String.concat ~sep:"\n" (List.map ~f:Lp.Cnstr.to_string constraints));
  (make (maximize z) constraints, z)

(* copied from ocaml-lp, available in 0.5.0 *)

let compute_term pmap t =
  let open Lp in
  match t with
  | Term.Const f -> f
  | Term.Linear (f, x) -> f *. PMap.find (Poly.of_var x) pmap
  | Term.Quad (f, x, y) ->
      f *. PMap.find (Poly.of_var x) pmap *. PMap.find (Poly.of_var y) pmap

let compute_poly pmap p =
  Lp.Poly.map (compute_term pmap) p |> List.fold ~f:( +. ) ~init:0.

let part3 l =
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
  | Plant (final_pid, _, _) -> (
      let problem, z = build plants_by_id final_pid in
      Stdio.printf "problem: %s\n" (Lp.Problem.to_string problem);
      match Lp_glpk.Milp.solve problem with
      | Ok (_, pmap) ->
          Stdio.printf "%s\n"
            (String.concat ~sep:"\n"
               (List.map
                  ~f:(fun (p, f) ->
                    Printf.sprintf "%s=%.2f" (Lp.Poly.to_string p) f)
                  (Lp.PMap.to_list pmap)));
          Int.of_float (compute_poly pmap z)
      | Error msg ->
          Stdio.printf "Failed to solve: %s\n" msg;
          assert false)
