val read_lines : string -> string list
val concat_ints : int list -> int

val mod_positive : int -> int -> int
val mod_exp: int -> int -> int -> int
val sum_list: int list -> int
val (--): int -> int -> int list
val set_add_all: ('a, 'b) Base.Set.t -> 'a list -> ('a, 'b) Base.Set.t
val string_to_int_list: ?on:char -> string -> int list
