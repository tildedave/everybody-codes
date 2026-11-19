open Base
open Core

let last_group_idx s lo hi ch =
  let i = ref (hi - 1) in
  with_return (fun r ->
      while !i >= lo do
        if equal_char s.[!i] ch then r.return (Some !i);
        i := !i - 1
      done;
      r.return None)

let%test_unit "last_group_idx" =
  [%test_result: int option] ~expect:(Some 1) (last_group_idx "ABCD" 0 4 'B')

let%test_unit "last_group_idx" =
  [%test_result: int option] ~expect:(Some 3) (last_group_idx "ABBBCD" 0 6 'B')

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
                lo hi
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
