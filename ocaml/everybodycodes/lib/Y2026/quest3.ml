open Base

let string_to_nums s =
  s
  |> String.fold ~init:[] ~f:(fun acc ch ->
      (Int.of_string @@ Char.to_string ch) :: acc)
  |> List.rev

let%test_unit "string_to_nums" =
  [%test_eq: int list] [ 3; 4; 5; 2 ] (string_to_nums "3452")

let nums s =
  match String.split ~on:' ' @@ List.nth_exn (String.split s ~on:'-') 1 with
  | [ first; second ] ->
      let first_nums = string_to_nums first in
      let len = List.length first_nums in
      List.zip_exn
        (List.mapi first_nums ~f:(fun i n -> (len - 1 - i, n)))
        (string_to_nums second)
      |> List.map ~f:(fun ((pow, base), digit) -> (base ** pow) * digit)
      |> List.fold ~init:0 ~f:( + )
  | _ -> failwith "invalid"

let%test_unit "nums" =
  [%test_eq: int list] [ 222; 1252; 4 ]
    (List.map ~f:nums [ "ISO-3210 7654"; "ISO-8742 2468"; "ISO-1111 1111" ])

let part1 l = List.fold ~f:( + ) ~init:0 @@ List.map ~f:nums l
let part2 _ = 1
let part3 _ = 1
