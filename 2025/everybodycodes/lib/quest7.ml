open Base

let parse_suffixes m s =
  Stdlib.Scanf.sscanf s "%c > %s" (fun ch ll ->
      Map.add_exn m ~key:ch
        ~data:
          (Util.set_add_all
             (Base.Set.empty (module Char))
             (List.map ~f:(fun s -> String.get s 0) (String.split ~on:',' ll))))

let char_pairs s =
  List.drop (String.to_list s) 1
  |> List.fold
       ~init:(String.get s 0, [])
       ~f:(fun (last, sofar) next -> (next, (last, next) :: sofar))
  |> snd |> List.rev

let%test_unit "char_pairs" =
  [%test_eq: (char * char) list]
    [ ('O', 'r'); ('r', 'o'); ('o', 'n'); ('n', 'r'); ('r', 'i'); ('i', 's') ]
    (char_pairs "Oronris")

let is_valid m name =
  List.fold
    ~f:(fun valid (ch1, ch2) -> valid && Set.mem (Map.find_exn m ch1) ch2)
    ~init:true (char_pairs name)

let part1 l =
  let names, suffixes = (String.split ~on:',' (List.hd_exn l), List.drop l 2) in
  let m =
    List.fold ~f:parse_suffixes ~init:(Base.Map.empty (module Char)) suffixes
  in
  names |> List.filter ~f:(is_valid m) |> List.hd_exn

let part2 l =
  let names, suffixes = (String.split ~on:',' (List.hd_exn l), List.drop l 2) in
  let m =
    List.fold ~f:parse_suffixes ~init:(Base.Map.empty (module Char)) suffixes
  in
  names
  |> List.fold ~init:(1, 0) ~f:(fun (idx, acc) s ->
         (idx + 1, if is_valid m s then acc + idx else acc))
  |> snd

let%test_unit "part1 given" =
  [%test_eq: string] "Oroneth"
    (part1
       [
         "Oronris,Urakris,Oroneth,Uraketh";
         "";
         "r > a,i,o";
         "i > p,w";
         "n > e,r";
         "o > n,m";
         "k > f,r";
         "a > k";
         "U > r";
         "e > t";
         "O > r";
         "t > h";
       ])

let%test_unit "part2 given" =
  [%test_eq: int] 23
    (part2
       [
         "Xanverax,Khargyth,Nexzeth,Helther,Braerex,Tirgryph,Kharverax";
         "";
         "r > v,e,a,g,y";
         "a > e,v,x,r";
         "e > r,x,v,t";
         "h > a,e,v";
         "g > r,y";
         "y > p,t";
         "i > v,r";
         "K > h";
         "v > e";
         "B > r";
         "t > h";
         "N > e";
         "p > h";
         "H > e";
         "l > t";
         "z > e";
         "X > a";
         "n > v";
         "x > z";
         "T > i";
       ])

let last_char s = String.get s (String.length s - 1)
let%test_unit "last_char" = [%test_eq: char] 't' (last_char "Xaryt")

let rec expand ~mapping s ~prefix ~min_length ~max_length =
  Stdio.printf ">> %s <<\n" prefix;
  Set.fold ~init:s
    ~f:(fun sofar next_ch ->
      let next_string = String.append prefix (Char.to_string next_ch) in
      if Set.mem sofar next_string || String.length next_string > max_length
      then sofar
      else
        expand ~mapping ~prefix:next_string
          (if String.length next_string < min_length then sofar
           else Set.add sofar next_string)
          ~max_length ~min_length)
    (Option.value
       (Map.find mapping (last_char prefix))
       ~default:(Set.empty (module Char)))

let part3 l =
  let names, suffixes = (String.split ~on:',' (List.hd_exn l), List.drop l 2) in
  let m =
    List.fold ~f:parse_suffixes ~init:(Base.Map.empty (module Char)) suffixes
  in
  Stdio.printf ">> %s <<\n" "joe";
  names
  |> List.filter ~f:(is_valid m)
  |> List.fold ~init:0 ~f:(fun acc name ->
         acc
         + Set.count
             ~f:(fun _ -> true)
             (expand ~mapping:m ~prefix:name
                (Set.empty (module String))
                ~max_length:11 ~min_length:7))

let%test_unit "part3 given" =
  [%test_eq: int] 25
    (part3
       [
         "Xaryt";
         "";
         "X > a,o";
         "a > r,t";
         "r > y,e,a";
         "h > a,e,v";
         "t > h";
         "v > e";
         "y > p,t";
       ])

let%test_unit "part3 given (2)" =
  [%test_eq: int] 1154
    (part3
       [
         "Khara,Xaryt,Noxer,Kharax";
         "";
         "r > v,e,a,g,y";
         "a > e,v,x,r,g";
         "e > r,x,v,t";
         "h > a,e,v";
         "g > r,y";
         "y > p,t";
         "i > v,r";
         "K > h";
         "v > e";
         "B > r";
         "t > h";
         "N > e";
         "p > h";
         "H > e";
         "l > t";
         "z > e";
         "X > a";
         "n > v";
         "x > z";
         "T > i";
       ])
