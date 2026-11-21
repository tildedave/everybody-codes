open Base
open Core

(* I guess we'll try to make a linked list for this *)
(* probably some cooler way exists *)
(*
let _id = ref 0

let next_id () =
  _id := !_id + 1;
  !_id

type node = {
  id : int;
  mutable lo : int;
  mutable hi : int;
  mutable left : node option;
  mutable right : node option;
}
[@@deriving sexp_of, compare, show, equal]

let make_node lo hi = { id = next_id (); lo; hi; left = None; right = None }

let pair_node node1 node2 =
  node1.right <- Some node2;
  node2.left <- Some node1

let flip node =
  let old_lo = node.lo in
  node.lo <- node.hi;
  node.hi <- old_lo

let reverse_chain start_node end_node =
  (* at the end:
         start_node and end_node are both flipped
         end_node->prev = start_node->prev
         start_node->next = end_node->next
         and everything in the middle is flipped too
    *)
  let curr = ref start_node in
  let is_end node =
    (* either node.right is None, or it's Some and = end_node.right's ID *)
    match node.right with
    | None -> true
    | Some n -> (
        match end_node.right with
        | None ->
            failwith
              "I don't think this is possible (would have required iterating \
               past end_node)"
        | Some m -> equal_int n.id m.id)
  in
  while not (is_end !curr) do
    let next_node, prev_node = (!curr.right, !curr.left) in
    !curr.right <- prev_node;
    !curr.left <- next_node;

    (* this doesn't rewrite the left pointers, which we need to do too *)
    curr := Option.value_exn next_node (* must be non-None due to is_end *)
  done;
  match start_node.left with None -> () | Some n -> n.right <- Some !curr

let%test_unit "reverse_chain" =
  let n1, n2, n3 = (make_node 1 3, make_node 4 5, make_node 5 10) in
  [%test_result: node]
    ~expect:{ lo = 1; hi = 2; left = None; right = None; id = 1 }
    begin
      pair_node n1 n2;
      pair_node n2 n3;
      reverse_chain n1 n3
    end *)

(* let direction node = if node.lo < node.hi then 1 else -1 *)

(* OK so ranges need to be inclusive for this to make any sense, otherwise
   flipping is hard *)

let in_range x (lo, hi) = min lo hi <= x && x <= max lo hi

let last_group_idx s lo hi ch =
  let i = ref (hi - 1) in
  with_return (fun r ->
      while in_range !i (lo, hi) do
        if equal_char s.[!i] ch then r.return (Some !i);
        i := !i - 1
      done;
      r.return None)

let%test_unit "last_group_idx" =
  [%test_result: int option] ~expect:(Some 1) (last_group_idx "ABCD" 0 3 'B')

let%test_unit "last_group_idx (second)" =
  [%test_result: int option] ~expect:(Some 3) (last_group_idx "ABBBCD" 0 5 'B')

(* what do I even want the answer to this to be?  we don't care about indices
   OK, we want to split it *)

let make_interval (lo, hi, dx) =
  match dx with
  | -1 -> if hi > lo then None else Some (lo, hi, dx)
  | 1 -> if lo > hi then None else Some (lo, hi, dx)
  | _ -> failwith "invalid dx"

(* intention: (5, 0, -1) in ABBBCE is traversed (forwards) ECBBBA and
   (backwards) ABBBCE *)

let split_on_last_idx_of s (lo, hi, dx) ch =
  let i = ref hi in
  with_return (fun r ->
      while in_range !i (lo, hi) do
        if equal_char s.[!i] ch then (* these intervals may be empty *)
          [ make_interval (lo, !i, dx); make_interval (!i + dx, hi, dx) ]
          |> List.filter_opt |> Some |> r.return;
        i := !i - dx
      done;
      r.return None)

let%test_unit "split_on_last_idx_of (forward)" =
  [%test_result: (int * int * int) list option]
    ~expect:(Some [ (0, 3, 1); (4, 5, 1) ])
    (split_on_last_idx_of "ABBBCE" (0, 5, 1) 'B')

let%test_unit "split_on_last_idx_of (reverse)" =
  [%test_result: (int * int * int) list option]
    ~expect:(Some [ (5, 1, -1); (0, 0, -1) ])
    (split_on_last_idx_of "ABBBCE" (5, 0, -1) 'B')

let first_different s lo hi =
  let i = ref (lo + 1) in
  let ch = s.[lo] in
  with_return (fun r ->
      while !i < hi do
        if not (equal_char s.[!i] ch) then r.return (Some !i);
        i := !i + 1
      done;
      r.return None)

let%test_unit "first_different (None)" =
  [%test_result: int option] ~expect:None (first_different "AAABBBCC" 0 3)

let%test_unit "first_different (Some)" =
  [%test_result: int option] ~expect:(Some 3) (first_different "AAABBBCC" 0 6)

let arrange s =
  let rec helper s lo hi flip_length flip_count =
    if lo >= hi then (s, flip_count, flip_length)
    else
      let ch = s.[lo] in
      let last_ch_idx = last_group_idx s lo hi ch in
      match last_ch_idx with
      | None -> failwith "impossible"
      | Some n -> (
          match first_different s lo (n + 1) with
          | None -> helper s (n + 1) hi flip_length flip_count
          | Some m ->
              let prefix = String.sub s ~pos:0 ~len:m in
              (* flip m ---> n + 1. last_ch_idx is wrong *)
              let flip_area = String.sub s ~pos:m ~len:(n + 1 - m) in
              let suffix = String.sub s ~pos:(n + 1) ~len:(hi - (n + 1)) in
              (* Stdio.printf "\tprefix = %s, flip area = %s, suffix = %s\n" prefix
              flip_area suffix; *)
              helper
                (String.append
                   (String.append prefix (String.rev flip_area))
                   suffix)
                (m - 1) hi
                (flip_length + String.length flip_area)
                (flip_count + 1))
  in
  helper s 0 (String.length s) 0 0

let%test_unit "arrange" =
  [%test_result: string * int * int] ~expect:("RRRRGGGGGGBBBB", 3, 22)
    (arrange "RRGGBBGRRGGBBG")

let%test_unit "arrange (example 2)" =
  [%test_result: string * int * int]
    ~expect:
      ( "BBBBBBBBBBBBBBBBBBBBBBGGGGGGGGGGGGGGGGGGGGGGRRRRRRRRRRRRRRRRRRRRRRRRRR",
        26,
        838 )
    (arrange
       "BBRGGRRGBBRGGBRGBBRRBRRRBGGRRRBGBGGBBRGGRRGBBRGGBRGBBRRBRRRBGGRRRBGBGG")

let part1 s = s |> arrange |> fun (_, n, _) -> n

let part2 s =
  String.concat (List.init 10 ~f:(fun _ -> s)) |> arrange |> fun (_, _, l) -> l

let%test_unit "part2" = [%test_result: int] ~expect:1053 (part2 "RRYGYBYG")

let part3 s =
  String.concat (List.init 100 ~f:(fun _ -> s)) |> arrange |> fun (_, _, l) -> l

let%test_unit "part3" = [%test_result: int] ~expect:135154 (part3 "RRYGGYBBYG")
