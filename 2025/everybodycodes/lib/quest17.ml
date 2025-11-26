open Base
open Util

let num_destroyed r destroyed grid =
  let xv, yv = grid |> grid_find ~f:(equal_char '@') |> Option.value_exn in
  grid_fold grid ~init:(0, destroyed) ~f:(fun (xc, yc) (count, destroyed) ch ->
      if equal_tuple (xc, yc) (xv, yv) then (count, destroyed)
      else if Set.mem destroyed (xc, yc) then (count, destroyed)
      else if ((xv - xc) * (xv - xc)) + ((yv - yc) * (yv - yc)) <= r * r then
        (count + num_of_char ch, Set.add destroyed (xc, yc))
      else (count, destroyed))

let destruction_sequence grid =
  Sequence.unfold
    ~init:(1, Set.empty (module IntPair_Comparator))
    ~f:(fun (r, destroyed) ->
      let count, next_destroyed = num_destroyed r destroyed grid in
      Some (count, (r + 1, next_destroyed)))

let part1 l =
  let grid = to_grid l in
  fst @@ num_destroyed 10 (Set.empty (module IntPair_Comparator)) grid

let%test_unit "part1 (given)" =
  [%test_eq: int] 1573
    (part1
       [
         "189482189843433862719";
         "279415473483436249988";
         "432746714658787816631";
         "428219317375373724944";
         "938163982835287292238";
         "627369424372196193484";
         "539825864246487765271";
         "517475755641128575965";
         "685934212385479112825";
         "815992793826881115341";
         "1737798467@7983146242";
         "867597735651751839244";
         "868364647534879928345";
         "519348954366296559425";
         "134425275832833829382";
         "764324337429656245499";
         "654662236199275446914";
         "317179356373398118618";
         "542673939694417586329";
         "987342622289291613318";
         "971977649141188759131";
       ])

let part2 l =
  let grid = to_grid l in
  destruction_sequence grid
  |> Sequence.take_while ~f:(fun n -> n <> 0)
  |> Sequence.mapi ~f:(fun n x -> (n + 1, x))
  |> Sequence.max_elt ~compare:(fun (_, x) (_, y) -> compare x y)
  |> Option.value_exn
  |> fun (x, y) -> x * y

let%test_unit "part2 (given)" =
  [%test_eq: int] 1090
    (part2
       [
         "4547488458944";
         "9786999467759";
         "6969499575989";
         "7775645848998";
         "6659696497857";
         "5569777444746";
         "968586@767979";
         "6476956899989";
         "5659745697598";
         "6874989897744";
         "6479994574886";
         "6694118785585";
         "9568991647449";
       ])

let part3 _ = 1
