(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open Everybodycodes;;

print_endline (match (Sys.argv.(1), Sys.argv.(2)) with
  | ("quest1", "1") -> Quest1.part1 (Util.read_lines Sys.argv.(3))
  | ("quest1", "2") -> Quest1.part2 (Util.read_lines Sys.argv.(3))
  | ("quest1", "3") -> Quest1.part3 (Util.read_lines Sys.argv.(3))
  | _ -> failwith "invalid argument");;
