open Base
open Core
open Util

type board = Board of (int * int) * string

let parse_lines l =
  let idx = fst @@ List.findi_exn ~f:(fun _ s -> String.equal "" s) l in
  match List.split_n l idx with
  | l1, l2 ->
      ( Board
          ((String.length (List.hd_exn l1), List.length l1), String.concat l1),
        List.drop l2 1 )

let board_drop (Board ((xmax, ymax), grid)) token slot =
  let x, y, token_idx = (ref ((slot - 1) * 2), ref 0, ref 0) in
  while !y < ymax do
    (* Stdio.printf "[%d] x=%d, y=%d %c %c\n" slot !x !y *)
    (* grid.[(!y * xmax) + !x]
       token.[!token_idx]; *)
    (match grid.[(!y * xmax) + !x] with
    | '.' -> ()
    | '*' ->
        (match token.[!token_idx] with
        | 'L' -> x := if !x = 0 then 1 else !x - 1
        | 'R' -> x := if !x = xmax - 1 then xmax - 2 else !x + 1
        | _ -> failwith "invalid token");
        token_idx := !token_idx + 1
    | _ -> failwith "invalid grid entry");
    y := !y + 1
  done;
  (* Stdio.printf "[%d - final] x=%d, y=%d\n" slot !x !y; *)
  (!x / 2) + 1

let test_board =
  [
    "*.*.*.*.*";
    ".*.*.*.*.";
    "*...*.*.*";
    ".*.*...*.";
    "*.*...*..";
    ".*.*.*.*.";
    "";
  ]

let%test_unit "quest 1 part1 (given)" =
  [%test_eq: int] 4
    (let board, _ = parse_lines test_board in
     board_drop board "RLLRLR" 5)

let%test_unit "quest 1 part1 (given 2)" =
  [%test_eq: int] 4
    (let board, _ = parse_lines test_board in
     board_drop board "RLLRLR" 4)

let test_board =
  [
    "*.*.*.*.*.*.*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "*.*.*...*.*...*..";
    ".*.*.*.*.*...*.*.";
    "*.*.....*...*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "*...*...*.*.*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "*.*.*...*.*.*.*.*";
    ".*...*...*.*.*.*.";
    "*.*.*.*.*.*.*.*.*";
    ".*.*.*.*.*.*.*.*.";
    "";
    "RRRLRLRRRRRL";
    "LLLLRLRRRRRR";
    "RLLLLLRLRLRL";
    "LRLLLRRRLRLR";
    "LLRLLRLLLRRL";
    "LRLRLLLRRRRL";
    "LRLLLLLLRLLL";
    "RRLLLRLLRLRR";
    "RLLLLLRLLLRL";
  ]

let tokens_won board token slot_num =
  max 0 ((board_drop board token slot_num * 2) - slot_num)

let quest1part1 l =
  let board, tokens = parse_lines l in
  tokens
  |> List.mapi ~f:(fun n token -> tokens_won board token (n + 1))
  |> List.fold ~f:( + ) ~init:0

let%test_unit "quest 1 quest1part1 (coins won)" =
  [%test_eq: int] 26 (quest1part1 test_board)

let quest1part2 l =
  let board, tokens = parse_lines l in
  match board with
  | Board ((xmax, _), _) ->
      tokens
      |> List.map ~f:(fun token ->
             1 -- ((xmax + 1) / 2)
             |> List.map ~f:(fun slot_num -> tokens_won board token slot_num)
             |> List.max_elt ~compare |> Option.value_exn)
      |> List.fold ~f:( + ) ~init:0

let%test_unit "quest1 quest1part2 (coins won)" =
  [%test_eq: int] 115
    (quest1part2
       [
         "*.*.*.*.*.*.*.*.*.*.*.*.*";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "..*.*.*.*...*.*...*.*.*..";
         ".*...*.*.*.*.*.*.....*.*.";
         "*.*...*.*.*.*.*.*...*.*.*";
         ".*.*.*.*.*.*.*.*.......*.";
         "*.*.*.*.*.*.*.*.*.*...*..";
         ".*.*.*.*.*.*.*.*.....*.*.";
         "*.*...*.*.*.*.*.*.*.*....";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "*.*.*.*.*.*.*.*.*.*.*.*.*";
         ".*.*.*.*.*.*.*.*.*...*.*.";
         "*.*.*.*.*.*.*.*.*...*.*.*";
         ".*.*.*.*.*.*.*.*.....*.*.";
         "*.*.*.*.*.*.*.*...*...*.*";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "*.*.*...*.*.*.*.*.*.*.*.*";
         ".*...*.*.*.*...*.*.*...*.";
         "*.*.*.*.*.*.*.*.*.*.*.*.*";
         ".*.*.*.*.*.*.*.*.*.*.*.*.";
         "";
         "RRRLLRRRLLRLRRLLLRLR";
         "RRRRRRRRRRLRRRRRLLRR";
         "LLLLLLLLRLRRLLRRLRLL";
         "RRRLLRRRLLRLLRLLLRRL";
         "RLRLLLRRLRRRLRRLRRRL";
         "LLLLLLLLRLLRRLLRLLLL";
         "LRLLRRLRLLLLLLLRLRRL";
         "LRLLRRLLLRRRRRLRRLRR";
         "LRLLRRLRLLRLRRLLLRLL";
         "RLLRRRRLRLRLRLRLLRRL";
       ])

(* part 3 is brute forceable *)

(*
OK, run each token and figure out what its values are for each slot
then find a bipartite matching of the graph
bipartite matching algo is basically max flow/min cuts
this is a pretty straightforward algo
*)

module Node = struct
  type t = int [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

module Uid = Unique_id.Int ()

module Edge = struct
  type t = Uid.t * Uid.t [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

let build_graph board tokens =
  match board with
  | Board ((xmax, _), _) ->
      let tokens_to_node, slots_to_node =
        (Hashtbl.create (module String), Hashtbl.create (module Int))
      in
      let source, sink = (Uid.create (), Uid.create ()) in
      let edges = Hashtbl.create (module Edge) in
      List.iter
        ~f:(fun token ->
          Hashtbl.set tokens_to_node ~key:token ~data:(Uid.create ()))
        tokens;
      List.iter
        ~f:(fun slot ->
          Hashtbl.set slots_to_node ~key:slot ~data:(Uid.create ()))
        (1 -- ((xmax + 1) / 2));
      (* 1k is just a high value that lets our algo ignore source/sink limiting *)
      Hashtbl.iter tokens_to_node ~f:(fun token_node ->
          Hashtbl.set edges ~key:(source, token_node) ~data:1000);
      Hashtbl.iter slots_to_node ~f:(fun slot_node ->
          Hashtbl.set edges ~key:(slot_node, sink) ~data:1000);
      List.iter
        ~f:(fun (token, slot) ->
          let token_node, slot_node =
            ( Hashtbl.find_exn tokens_to_node token,
              Hashtbl.find_exn slots_to_node slot )
          in
          Hashtbl.set edges ~key:(token_node, slot_node)
            ~data:(tokens_won board token slot))
        (List.cartesian_product
           (Hashtbl.keys tokens_to_node)
           (Hashtbl.keys slots_to_node));
      ((source, sink), tokens_to_node, slots_to_node, edges)

let find_default htbl ~key ~default =
  match Hashtbl.find htbl key with None -> default | Some s -> s

let residual_neighbors v (flow : (Edge.t, int) Base.Hashtbl.t)
    (capacity : (Edge.t, int) Base.Hashtbl.t) =
  (* this is a quite inefficient method of doing it, but whatever. *)
  (* data sizes are so tiny it doesn't matter *)
  Hashtbl.keys flow
  |> List.filter ~f:(fun (e1, _) -> Uid.equal e1 v)
  |> List.filter ~f:(fun (e1, e2) ->
         find_default capacity ~key:(e1, e2) ~default:0
         - find_default flow ~key:(e1, e2) ~default:0
         > 0)
  |> List.map ~f:snd

let find_path (source, sink) flow capacity =
  let explored = Hash_set.create (module Uid) in
  let parent = Hashtbl.create (module Uid) in
  let rec build_path node path =
    if Uid.equal node source then source :: path
    else build_path (Hashtbl.find_exn parent node) (node :: path)
  in
  let queue = Queue.create () in
  Queue.enqueue queue source;
  Hash_set.add explored source;
  let rec loop () =
    match Queue.dequeue queue with
    | None -> None
    | Some v ->
        if Uid.equal v sink then Some (build_path sink [])
        else (
          List.iter
            ~f:(fun w ->
              if not (Hash_set.mem explored w) then (
                Hash_set.add explored w;
                Hashtbl.set parent ~key:w ~data:v;
                Queue.enqueue queue w))
            (residual_neighbors v flow capacity);
          loop ())
  in
  loop ()

let path_to_edges path =
  List.tl_exn path
  |> List.fold
       ~init:(List.hd_exn path, [])
       ~f:(fun (last, sofar) next -> (next, (last, next) :: sofar))
  |> snd

let ford_fulkerson board tokens =
  let (source, sink), tokens_to_node, slots_to_node, capacity =
    build_graph board tokens
  in
  let flow = Hashtbl.create (module Edge) in
  Hashtbl.iter_keys capacity ~f:(fun (e1, e2) ->
      Hashtbl.set flow ~key:(e1, e2) ~data:0);
  let rec loop () =
    match find_path (source, sink) flow capacity with
    | None -> Stdio.printf "done"
    | Some path ->
        let path_edges = path_to_edges path in
        let min_flow =
          path_edges
          |> List.map ~f:(fun e -> Hashtbl.find_exn capacity e)
          |> List.max_elt ~compare |> Option.value_exn
        in
        List.iter
          ~f:(fun (e1, e2) ->
            Hashtbl.set flow ~key:(e1, e2)
              ~data:(find_default flow ~key:(e1, e2) ~default:0 + min_flow);
            Hashtbl.set flow ~key:(e2, e1)
              ~data:(find_default flow ~key:(e2, e1) ~default:0 - min_flow))
          path_edges
  in
  loop ()
