(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open Base
open Everybodycodes

let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines
;;

Stdio.print_endline
  (let argv = Sys.get_argv () in
   let lines = read_lines argv.(4) in
   match (argv.(1), argv.(2), argv.(3)) with
   | "2025", "quest1", "1" -> Y2025.Quest1.part1 lines
   | "2025", "quest1", "2" -> Y2025.Quest1.part2 lines
   | "2025", "quest1", "3" -> Y2025.Quest1.part3 lines
   | "2025", "quest2", "1" -> Y2025.Quest2.part1 @@ List.hd_exn lines
   | "2025", "quest2", "2" -> Y2025.Quest2.part2 @@ List.hd_exn lines
   | "2025", "quest2", "3" -> Y2025.Quest2.part3 @@ List.hd_exn lines
   | "2025", "quest3", "1" -> Y2025.Quest3.part1 @@ List.hd_exn lines
   | "2025", "quest3", "2" -> Y2025.Quest3.part2 @@ List.hd_exn lines
   | "2025", "quest3", "3" -> Y2025.Quest3.part3 @@ List.hd_exn lines
   | "2025", "quest4", "1" ->
       Int.to_string @@ Y2025.Quest4.part1 (List.map ~f:Int.of_string lines)
   | "2025", "quest4", "2" ->
       Int.to_string @@ Y2025.Quest4.part2 (List.map ~f:Int.of_string lines)
   | "2025", "quest4", "3" -> Int.to_string @@ Y2025.Quest4.part3 lines
   | "2025", "quest5", "1" ->
       Int.to_string @@ Y2025.Quest5.part1 @@ List.hd_exn lines
   | "2025", "quest5", "2" -> Y2025.Quest5.part2 @@ lines
   | "2025", "quest5", "3" -> Int.to_string @@ Y2025.Quest5.part3 lines
   | "2025", "quest6", "1" ->
       Int.to_string @@ Y2025.Quest6.part1 @@ List.hd_exn lines
   | "2025", "quest6", "2" ->
       Int.to_string @@ Y2025.Quest6.part2 @@ List.hd_exn lines
   | "2025", "quest6", "3" ->
       Int.to_string
       @@ Y2025.Quest6.part3 (List.hd_exn lines) ~num_repeats:1000
            ~distance:1000
   | "2025", "quest7", "1" -> Y2025.Quest7.part1 lines
   | "2025", "quest7", "2" -> Int.to_string @@ Y2025.Quest7.part2 lines
   | "2025", "quest7", "3" -> Int.to_string @@ Y2025.Quest7.part3 lines
   | "2025", "quest8", "1" ->
       Int.to_string @@ Y2025.Quest8.part1 @@ List.hd_exn lines
   | "2025", "quest8", "2" ->
       Int.to_string @@ Y2025.Quest8.part2 @@ List.hd_exn lines
   | "2025", "quest8", "3" ->
       Int.to_string @@ Y2025.Quest8.part3 @@ List.hd_exn lines
   | "2025", "quest9", "1" -> Int.to_string @@ Y2025.Quest9.part1 lines
   | "2025", "quest9", "2" -> Int.to_string @@ Y2025.Quest9.part2 lines
   | "2025", "quest9", "3" -> Int.to_string @@ Y2025.Quest9.part3 lines
   | "2025", "quest10", "1" -> Int.to_string @@ Y2025.Quest10.part1 lines
   | "2025", "quest10", "2" -> Int.to_string @@ Y2025.Quest10.part2 lines
   | "2025", "quest10", "3" -> Int.to_string @@ Y2025.Quest10.part3 lines
   | "2025", "quest11", "1" -> Int.to_string @@ Y2025.Quest11.part1 lines
   | "2025", "quest11", "2" -> Int.to_string @@ Y2025.Quest11.part2 lines
   | "2025", "quest11", "3" -> Int.to_string @@ Y2025.Quest11.part3 lines
   | "2025", "quest12", "1" -> Int.to_string @@ Y2025.Quest12.part1 lines
   | "2025", "quest12", "2" -> Int.to_string @@ Y2025.Quest12.part2 lines
   | "2025", "quest12", "3" -> Int.to_string @@ Y2025.Quest12.part3 lines
   | "2025", "quest13", "1" -> Int.to_string @@ Y2025.Quest13.part1 lines
   | "2025", "quest13", "2" -> Int.to_string @@ Y2025.Quest13.part2 lines
   | "2025", "quest13", "3" -> Int.to_string @@ Y2025.Quest13.part3 lines
   | "2025", "quest14", "1" -> Int.to_string @@ Y2025.Quest14.part1 lines
   | "2025", "quest14", "2" -> Int.to_string @@ Y2025.Quest14.part2 lines
   | "2025", "quest14", "3" -> Int.to_string @@ Y2025.Quest14.part3 lines
   | "2025", "quest15", "1" ->
       Int.to_string @@ Y2025.Quest15.part1 @@ List.hd_exn lines
   | "2025", "quest15", "2" ->
       Int.to_string @@ Y2025.Quest15.part1 @@ List.hd_exn lines
   | "2025", "quest15", "3" ->
       Int.to_string @@ Y2025.Quest15.part3 @@ List.hd_exn lines
   | "2025", "quest16", "1" ->
       Int.to_string @@ Y2025.Quest16.part1 @@ List.hd_exn lines
   | "2025", "quest16", "2" ->
       Int.to_string @@ Y2025.Quest16.part2 @@ List.hd_exn lines
   | "2025", "quest16", "3" ->
       Int.to_string @@ Y2025.Quest16.part3 @@ List.hd_exn lines
   | "2025", "quest17", "1" -> Int.to_string @@ Y2025.Quest17.part1 lines
   | "2025", "quest17", "2" -> Int.to_string @@ Y2025.Quest17.part2 lines
   | "2025", "quest17", "3" -> Int.to_string @@ Y2025.Quest17.part3 lines
   | "2025", "quest18", "1" -> Int.to_string @@ Y2025.Quest18.part1 lines
   | "2025", "quest18", "2" -> Int.to_string @@ Y2025.Quest18.part2 lines
   | "2025", "quest18", "3" -> Int.to_string @@ Y2025.Quest18.part3 lines
   | "2025", "quest19", "1" -> Int.to_string @@ Y2025.Quest19.flap lines
   | "2025", "quest19", "2" -> Int.to_string @@ Y2025.Quest19.flap lines
   | "2025", "quest19", "3" -> Int.to_string @@ Y2025.Quest19.flap lines
   | "2025", "quest20", "1" -> Int.to_string @@ Y2025.Quest20.part1 lines
   | "2025", "quest20", "2" -> Int.to_string @@ Y2025.Quest20.part2 lines
   | "2025", "quest20", "3" -> Int.to_string @@ Y2025.Quest20.part3 lines
   | "2026", "quest1", "1" ->
       Int.to_string @@ Y2026.Quest1.part1 (List.hd_exn lines)
   | "2026", "quest1", "2" ->
       Int.to_string @@ Y2026.Quest1.part2 (List.hd_exn lines)
   | "2026", "quest1", "3" ->
       Int.to_string @@ Y2026.Quest1.part3 (List.hd_exn lines)
   | "2026", "quest2", "1" -> Int.to_string @@ Y2026.Quest2.part1 lines
   | "2026", "quest2", "2" -> Int.to_string @@ Y2026.Quest2.part2 lines
   | "2026", "quest2", "3" -> Int.to_string @@ Y2026.Quest2.part3 lines
   | "2026", "quest3", "1" -> Int.to_string @@ Y2026.Quest3.part1 lines
   | "2026", "quest3", "2" -> Int.to_string @@ Y2026.Quest3.part2 lines
   | "2026", "quest3", "3" -> Int.to_string @@ Y2026.Quest3.part3 lines
   | "2026", "quest4", "1" -> Int.to_string @@ Y2026.Quest4.part1 lines
   | "2026", "quest4", "2" -> Int.to_string @@ Y2026.Quest4.part2 lines
   | "2026", "quest4", "3" -> Int.to_string @@ Y2026.Quest4.part3 lines
   | "2026", "quest5", "1" -> Int.to_string @@ Y2026.Quest5.part1 lines
   | "2026", "quest5", "2" -> Int.to_string @@ Y2026.Quest5.part2 lines
   | "2026", "quest5", "3" -> Int.to_string @@ Y2026.Quest5.part3 lines
   | "2026", "quest6", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest6.part1
         | "2" -> Y2026.Quest6.part2
         | "3" -> Y2026.Quest6.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "2026", "quest7", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest7.part1
         | "2" -> Y2026.Quest7.part2
         | "3" -> Y2026.Quest7.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "2026", "quest8", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest8.part1
         | "2" -> Y2026.Quest8.part2
         | "3" -> Y2026.Quest8.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "2026", "quest9", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest9.part1
         | "2" -> Y2026.Quest9.part2
         | "3" -> Y2026.Quest9.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "2026", "quest10", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest10.part1
         | "2" -> Y2026.Quest10.part2
         | "3" -> Y2026.Quest10.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "2026", "quest11", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest11.part1
         | "2" -> Y2026.Quest11.part2
         | "3" -> Y2026.Quest11.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "2026", "quest12", p ->
       let f =
         match p with
         | "1" -> Y2026.Quest12.part1
         | "2" -> Y2026.Quest12.part2
         | "3" -> Y2026.Quest12.part3
         | _ -> failwith "impossible"
       in
       Int.to_string @@ f lines
   | "story1", "quest1", "1" -> Int.to_string @@ Story1.quest1part1 lines
   | "story1", "quest1", "2" -> Int.to_string @@ Story1.quest1part2 lines
   | "story1", "quest1", "3" -> Int.to_string @@ Story1.quest1part3 lines
   | "story1", "quest2", "1" -> Story1.quest2 lines
   | "story1", "quest2", "2" -> Story1.quest2 lines
   | "story1", "quest2", "3" -> Story1.quest2 ~part3:true lines
   | "story1", "quest3", "1" -> Int.to_string @@ Story1.quest3part1 lines
   | "story1", "quest3", "2" -> Int.to_string @@ Story1.quest3part2 lines
   | "story1", "quest3", "3" -> Int.to_string @@ Story1.quest3part2 lines
   | "story2", "quest1", "1" -> Int.to_string @@ Story2.quest1part1 lines
   | "story2", "quest1", "2" -> Int.to_string @@ Story2.quest1part2 lines
   | "story2", "quest1", "3" -> Story2.quest1part3 lines
   | "story2", "quest2", "1" ->
       Int.to_string @@ Story2.quest2part1 @@ List.hd_exn lines
   | "story2", "quest2", "2" ->
       Int.to_string @@ Story2.quest2part2 @@ List.hd_exn lines
   | "story2", "quest2", "3" ->
       Int.to_string @@ Story2.quest2part3 @@ List.hd_exn lines
   | "story2", "quest3", "1" -> Int.to_string @@ Story2.quest3part1 lines
   | "story2", "quest3", "2" -> Story2.quest3part2 lines
   | "story2", "quest3", "3" -> Int.to_string @@ Story2.quest3part3 lines
   | "story3", "quest1", "1" -> Int.to_string @@ Story3.quest1part1 lines
   | "story3", "quest1", "2" -> Int.to_string @@ Story3.quest1part2 lines
   | "story3", "quest1", "3" -> Int.to_string @@ Story3.quest1part3 lines
   | "story3", "quest2", "1" -> Int.to_string @@ Story3.quest2part1 lines
   | "story3", "quest2", "2" -> Int.to_string @@ Story3.quest2part2 lines
   | "story3", "quest2", "3" -> Int.to_string @@ Story3.quest2part3 lines
   | "story3", "quest3", "1" -> Int.to_string @@ Story3.quest3part1 lines
   | "story3", "quest3", "2" -> Int.to_string @@ Story3.quest3part2 lines
   | "story3", "quest3", "3" -> Int.to_string @@ Story3.quest3part3 lines
   | "story4", "quest1", "1" -> Int.to_string @@ Story4.quest1part1 lines
   | "story4", "quest1", "2" -> Int.to_string @@ Story4.quest1part2 lines
   | "story4", "quest1", "3" -> Int.to_string @@ Story4.quest1part3 lines
   | "story4", "quest2", "1" -> Int.to_string @@ Story4.quest2part1 lines
   | "story4", "quest2", "2" -> Int.to_string @@ Story4.quest2part2 lines
   | "story4", "quest2", "3" -> Int.to_string @@ Story4.quest2part3 lines
   | "story4", "quest3", "1" -> Int.to_string @@ Story4.quest3part1 lines
   | "story4", "quest3", "2" -> Int.to_string @@ Story4.quest3part2 lines
   | _ -> failwith "invalid argument")
