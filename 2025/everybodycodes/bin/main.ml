(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open Base
open Everybodycodes

let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines
;;

Stdio.print_endline
  (let argv = Sys.get_argv () in
   let lines = read_lines argv.(3) in
   match (argv.(1), argv.(2)) with
   | "quest1", "1" -> Quest1.part1 lines
   | "quest1", "2" -> Quest1.part2 lines
   | "quest1", "3" -> Quest1.part3 lines
   | "quest2", "1" -> Quest2.part1 @@ List.hd_exn lines
   | "quest2", "2" -> Quest2.part2 @@ List.hd_exn lines
   | "quest2", "3" -> Quest2.part3 @@ List.hd_exn lines
   | "quest3", "1" -> Quest3.part1 @@ List.hd_exn lines
   | "quest3", "2" -> Quest3.part2 @@ List.hd_exn lines
   | "quest3", "3" -> Quest3.part3 @@ List.hd_exn lines
   | "quest4", "1" ->
       Int.to_string @@ Quest4.part1 (List.map ~f:Int.of_string lines)
   | "quest4", "2" ->
       Int.to_string @@ Quest4.part2 (List.map ~f:Int.of_string lines)
   | "quest4", "3" -> Int.to_string @@ Quest4.part3 lines
   | "quest5", "1" -> Int.to_string @@ Quest5.part1 @@ List.hd_exn lines
   | "quest5", "2" -> Quest5.part2 @@ lines
   | "quest5", "3" -> Int.to_string @@ Quest5.part3 lines
   | "quest6", "1" -> Int.to_string @@ Quest6.part1 @@ List.hd_exn lines
   | "quest6", "2" -> Int.to_string @@ Quest6.part2 @@ List.hd_exn lines
   | "quest6", "3" ->
       Int.to_string
       @@ Quest6.part3 (List.hd_exn lines) ~num_repeats:1000 ~distance:1000
   | "quest7", "1" -> Quest7.part1 lines
   | "quest7", "2" -> Int.to_string @@ Quest7.part2 lines
   | "quest7", "3" -> Int.to_string @@ Quest7.part3 lines
   | "quest8", "1" -> Int.to_string @@ Quest8.part1 @@ List.hd_exn lines
   | "quest8", "2" -> Int.to_string @@ Quest8.part2 @@ List.hd_exn lines
   | "quest8", "3" -> Int.to_string @@ Quest8.part3 @@ List.hd_exn lines
   | "quest9", "1" -> Int.to_string @@ Quest9.part1 lines
   | "quest9", "2" -> Int.to_string @@ Quest9.part2 lines
   | "quest9", "3" -> Int.to_string @@ Quest9.part3 lines
   | "quest10", "1" -> Int.to_string @@ Quest10.part1 lines
   | "quest10", "2" -> Int.to_string @@ Quest10.part2 lines
   | "quest10", "3" -> Int.to_string @@ Quest10.part3 lines
   | "quest11", "1" -> Int.to_string @@ Quest11.part1 lines
   | "quest11", "2" -> Int.to_string @@ Quest11.part2 lines
   | "quest11", "3" -> Int.to_string @@ Quest11.part3 lines
   | "quest12", "1" -> Int.to_string @@ Quest12.part1 lines
   | "quest12", "2" -> Int.to_string @@ Quest12.part2 lines
   | "quest12", "3" -> Int.to_string @@ Quest12.part3 lines
   | "quest13", "1" -> Int.to_string @@ Quest13.part1 lines
   | "quest13", "2" -> Int.to_string @@ Quest13.part2 lines
   | "quest13", "3" -> Int.to_string @@ Quest13.part3 lines
   | "quest14", "1" -> Int.to_string @@ Quest14.part1 lines
   | "quest14", "2" -> Int.to_string @@ Quest14.part2 lines
   | "quest14", "3" -> Int.to_string @@ Quest14.part3 lines
   | "quest15", "1" -> Int.to_string @@ Quest15.part1 @@ List.hd_exn lines
   | "quest15", "2" -> Int.to_string @@ Quest15.part1 @@ List.hd_exn lines
   | "quest15", "3" -> Int.to_string @@ Quest15.part3 @@ List.hd_exn lines
   | "quest16", "1" -> Int.to_string @@ Quest16.part1 @@ List.hd_exn lines
   | "quest16", "2" -> Int.to_string @@ Quest16.part2 @@ List.hd_exn lines
   | "quest16", "3" -> Int.to_string @@ Quest16.part3 @@ List.hd_exn lines
   | "quest17", "1" -> Int.to_string @@ Quest17.part1 lines
   | "quest17", "2" -> Int.to_string @@ Quest17.part2 lines
   | "quest17", "3" -> Int.to_string @@ Quest17.part3 lines
   | "story1-quest1", "1" -> Int.to_string @@ Story1.quest1part1 lines
   | "story1-quest1", "2" -> Int.to_string @@ Story1.quest1part2 lines
   | "story1-quest1", "3" -> Int.to_string @@ Story1.quest1part3 lines
   | "story1-quest2", "1" -> Story1.quest2 lines
   | "story1-quest2", "2" -> Story1.quest2 lines
   | "story1-quest2", "3" -> Story1.quest2 ~part3:true lines
   | "story1-quest3", "1" -> Int.to_string @@ Story1.quest3part1 lines
   | "story1-quest3", "2" -> Int.to_string @@ Story1.quest3part2 lines
   | "story1-quest3", "3" -> Int.to_string @@ Story1.quest3part2 lines
   | "story2-quest1", "1" -> Int.to_string @@ Story2.quest1part1 lines
   | "story2-quest1", "2" -> Int.to_string @@ Story2.quest1part2 lines
   | "story2-quest1", "3" -> Story2.quest1part3 lines
   | "story2-quest2", "1" ->
       Int.to_string @@ Story2.quest2part1 @@ List.hd_exn lines
   | "story2-quest2", "2" ->
       Int.to_string @@ Story2.quest2part2 @@ List.hd_exn lines
   | "story2-quest2", "3" ->
       Int.to_string @@ Story2.quest2part3 @@ List.hd_exn lines
   | "story2-quest3", "1" -> Int.to_string @@ Story2.quest3part1 lines
   | "story2-quest3", "2" -> Story2.quest3part2 lines
   | "story2-quest3", "3" -> Int.to_string @@ Story2.quest3part3 lines
   | _ -> failwith "invalid argument")
