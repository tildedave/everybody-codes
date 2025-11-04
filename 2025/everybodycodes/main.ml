(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

open List;;

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines
