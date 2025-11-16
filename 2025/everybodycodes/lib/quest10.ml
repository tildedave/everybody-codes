open Base

let moves_from (max_x, max_y) (x, y) =
  List.filter
    ~f:(fun (x, y) -> x >= 0 && x < max_x && y >= 0 && y < max_y)
    [
      (x + 1, y + 2);
      (x - 1, y + 2);
      (x + 1, y - 2);
      (x - 1, y - 2);
      (x + 2, y + 1);
      (x + 2, y - 1);
      (x - 2, y + 1);
      (x - 2, y - 1);
    ]

let timedmoves_from bounds (n, (x, y)) =
  if equal n 0 then []
  else List.map ~f:(fun c -> (n - 1, c)) (moves_from bounds (x, y))

let char_at l (x, y) = String.get (List.nth_exn l y) x

let fold m ~init ~f ~neighbors start =
  let queue, visited, acc = (Queue.create (), Hash_set.create m, ref init) in
  Queue.enqueue queue start;
  while not (Queue.is_empty queue) do
    let next = Queue.dequeue_exn queue in
    if not (Hash_set.mem visited next) then (
      Hash_set.add visited next;
      acc := f !acc next;
      List.iter
        ~f:(fun neighbor ->
          if not (Hash_set.mem visited neighbor) then
            Queue.enqueue queue neighbor)
        (neighbors next))
  done

module Coord = struct
  type t = int * int [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

module SearchState = struct
  type t = int * (int * int) [@@deriving compare, sexp_of]

  let hash = Hashtbl.hash
end

(*
let _ =
  fold
    (module SearchState_Order)
    ~init:()
    ~f:(fun _ (n, (x, y)) -> Stdio.printf "[%d] (%d, %d)\n" n x y)
    ~neighbors:(timedmoves_from (10, 10))
    (4, (5, 6)) *)

(* this works but is overkill I guess.  we visit the same nodes separately because the time is in the state.  but whatever. *)

let bounds l = (String.length (List.hd_exn l), List.length l)

let _part1 num_moves l =
  let xmax, ymax = bounds l in
  let start =
    l
    |> List.find_mapi ~f:(fun y s ->
           match String.index s 'D' with None -> None | Some x -> Some (x, y))
    |> Option.value_exn
  in
  let hits = Hash_set.create (module Coord) in
  fold
    (module SearchState)
    ~init:()
    ~f:(fun _ (_, loc) ->
      if equal_char (char_at l loc) 'S' then Hash_set.add hits loc)
    ~neighbors:(timedmoves_from (xmax, ymax))
    (num_moves, start);
  Hash_set.length hits

let part1 = _part1 4

let%test_unit "part1 (given)" =
  [%test_eq: int] 27
    (_part1 3
       [
         "...SSS.......";
         ".S......S.SS.";
         "..S....S...S.";
         "..........SS.";
         "..SSSS...S...";
         ".....SS..S..S";
         "SS....D.S....";
         "S.S..S..S....";
         "....S.......S";
         ".SSS..SS.....";
         ".........S...";
         ".......S....S";
         "SS.....S..S..";
       ])
