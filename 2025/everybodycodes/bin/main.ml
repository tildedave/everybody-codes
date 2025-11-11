(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open Everybodycodes;;

print_endline
  (let lines = Util.read_lines Sys.argv.(3) in
   match (Sys.argv.(1), Sys.argv.(2)) with
   | "quest1", "1" -> Quest1.part1 lines
   | "quest1", "2" -> Quest1.part2 lines
   | "quest1", "3" -> Quest1.part3 lines
   | "quest2", "1" -> Quest2.part1 @@ List.hd lines
   | "quest2", "2" -> Quest2.part2 @@ List.hd lines
   | "quest2", "3" -> Quest2.part3 @@ List.hd lines
   | "quest3", "1" -> Quest3.part1 @@ List.hd lines
   | "quest3", "2" -> Quest3.part2 @@ List.hd lines
   | "quest3", "3" -> Quest3.part3 @@ List.hd lines
   | "quest4", "1" ->
       Int.to_string @@ Quest4.part1 (List.map int_of_string lines)
   | "quest4", "2" ->
       Int.to_string @@ Quest4.part2 (List.map int_of_string lines)
   | "quest4", "3" -> Int.to_string @@ Quest4.part3 lines
   | "quest5", "1" -> Int.to_string @@ Quest5.part1 @@ List.hd lines
   | "quest5", "2" -> Quest5.part2 @@ lines
   | "quest5", "3" -> Int.to_string @@ Quest5.part3 lines
   | "story1-quest1", "1" -> Int.to_string @@ Story1.quest1part1 lines
   | "story1-quest1", "2" -> Int.to_string @@ Story1.quest1part2 lines
   | "story1-quest1", "3" -> Int.to_string @@ Story1.quest1part3 lines
   | "quest6", "1" -> Int.to_string @@ Quest6.part1 @@ List.hd lines
   | "quest6", "2" -> Int.to_string @@ Quest6.part2 @@ List.hd lines
   | "quest6", "3" ->
       Int.to_string
       @@ Quest6.part3 (List.hd lines) ~num_repeats:1000 ~distance:1000
   | _ -> failwith "invalid argument")
