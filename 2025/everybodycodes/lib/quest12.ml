open Base
open Util

let bounds grid = (Array.length grid.(0), Array.length grid)
let grid_at grid (x, y) = grid.(y).(x)

let fold_coords grid ~f ~init =
  Array.foldi ~init
    ~f:(fun y acc arr ->
      Array.foldi ~init:acc ~f:(fun x acc ch -> f (x, y) acc ch) arr)
    grid

let neighbors grid (x, y) =
  let xmax, ymax = bounds grid in
  List.filter
    ~f:(fun (x, y) -> x >= 0 && x < xmax && y >= 0 && y < ymax)
    [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let explode_barrels grid exploded start_l =
  let queue = Queue.create () in
  List.iter start_l ~f:(fun start ->
      Hash_set.add exploded start;
      Queue.enqueue queue start);
  let rec loop num_destroyed =
    match Queue.dequeue queue with
    | None -> num_destroyed
    | Some (x, y) ->
        let curr_val = Int.of_string @@ Char.to_string @@ grid_at grid (x, y) in
        List.iter
          ~f:(fun next ->
            let next_val =
              Int.of_string @@ Char.to_string @@ grid_at grid next
            in
            if (not (Hash_set.mem exploded next)) && next_val <= curr_val then (
              Hash_set.add exploded next;
              Queue.enqueue queue next))
          (neighbors grid (x, y));
        loop (num_destroyed + 1)
  in
  loop 0

let part1 l =
  let grid = to_grid l in
  let exploded = Hash_set.create (module IntPair) in
  explode_barrels grid exploded [ (0, 0) ]

let%test_unit "part 1 (given)" =
  [%test_eq: int] 16 (part1 [ "989601"; "857782"; "746543"; "766789" ])

let part2 l =
  let grid = to_grid l in
  let xmax, ymax = bounds grid in
  let grid = to_grid l in
  let exploded = Hash_set.create (module IntPair) in
  explode_barrels grid exploded [ (0, 0); (xmax - 1, ymax - 1) ]

let%test_unit "part 2 (given)" =
  [%test_eq: int] 58
    (part2
       [
         "9589233445";
         "9679121695";
         "8469121876";
         "8352919876";
         "7342914327";
         "7234193437";
         "6789193538";
         "6781219648";
         "5691219769";
         "5443329859";
       ])

let part3 l =
  let grid = to_grid l in
  let find_winner last_best_exploded =
    fold_coords grid
      ~init:((0, 0), 0, Hash_set.create (module IntPair))
      ~f:(fun (x, y) (winner, winner_count, winner_exploded) _ ->
        let my_exploded = Hash_set.copy last_best_exploded in
        let my_count = explode_barrels grid my_exploded [ (x, y) ] in
        if my_count > winner_count then ((x, y), my_count, my_exploded)
        else (winner, winner_count, winner_exploded))
  in
  let best, _, best_exploded = find_winner (Hash_set.create (module IntPair)) in
  let second_best, _, second_best_exploded = find_winner best_exploded in
  let third_best, _, _ = find_winner second_best_exploded in
  explode_barrels grid
    (Hash_set.create (module IntPair))
    [ best; second_best; third_best ]

let%test_unit "part3 (given, 1)" =
  [%test_eq: int] 14 (part3 [ "5411"; "3362"; "5235"; "3112" ])

let%test_unit "part3 (given, 2)" =
  [%test_eq: int] 136
    (part3
       [
         "41951111131882511179";
         "32112222211518122215";
         "31223333322115122219";
         "31234444432147511128";
         "91223333322176121892";
         "61112222211166431583";
         "14661111166111111746";
         "11111119142122222177";
         "41222118881233333219";
         "71222127839122222196";
         "56111126279711111517";
       ])
