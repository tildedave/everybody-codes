(* https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/73019499#73019499 *)

let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

  let mod_positive n a = let m = n mod a in if m >= 0 then m else m + a;;

