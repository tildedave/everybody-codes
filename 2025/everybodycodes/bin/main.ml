(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open Everybodycodes;;

print_endline
  (let lines = Util.read_lines Sys.argv.(3) in
   match (Sys.argv.(1), Sys.argv.(2)) with
   | "quest1", "1" -> Quest1.part1 lines
   | "quest1", "2" -> Quest1.part2 lines
   | "quest1", "3" -> Quest1.part3 lines
   | "quest2", "1" -> Quest2.part1 (List.hd lines)
   | "quest2", "2" -> Quest2.part2 (List.hd lines)
   | "quest2", "3" -> Quest2.part3 (List.hd lines)
   | "quest3", "1" -> Quest3.part1 (List.hd lines)
   | "quest3", "2" -> Quest3.part2 (List.hd lines)
   | "quest3", "3" -> Quest3.part3 (List.hd lines)
   | _ -> failwith "invalid argument")
