open Base

let part1 l = 2025 * List.hd_exn l / List.last_exn l
let%test_unit "part1" = [%test_eq: int] 15888 (part1 [ 102; 75; 50; 35; 13 ])

let part2 l =
  let num, denom = (10000000000000 * List.last_exn l, List.hd_exn l) in
  if equal (num % denom) 0 then num / denom else (num / denom) + 1

let%test_unit "part2_p1" =
  [%test_eq: int] 1274509803922 (part2 [ 102; 75; 50; 35; 13 ])

let%test_unit "part2_p2" =
  [%test_eq: int] 625000000000 (part2 [ 128; 64; 32; 16; 8 ])

let part3 s =
  let num, denom =
    List.fold
      ~f:(fun (num, denom) curr ->
        if String.contains curr '|' then
          match List.map ~f:Float.of_string (String.split curr ~on:'|') with
          | [ left; right ] -> (num *. right, denom *. left)
          | _ -> failwith "invalid gear"
        else (num, denom *. Float.of_string curr))
      ~init:(Float.of_string @@ List.hd_exn s, Float.of_int 1)
      (List.tl_exn s)
  in
  Int.of_float @@ (100.0 *. num /. denom)

let%test_unit "part3_p1" =
  [%test_eq: int] 400 (part3 [ "5"; "5|10"; "10|20"; "5" ])

let%test_unit "part3_p2" =
  [%test_eq: int] 6818
    (part3 [ "5"; "7|21"; "18|36"; "27|27"; "10|50"; "10|50"; "11" ])
