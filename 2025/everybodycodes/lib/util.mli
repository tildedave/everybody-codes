val concat_ints : int list -> int
type ilist = int list
val show_ilist : int list -> string

val mod_positive : int -> int -> int
val mod_exp: int -> int -> int -> int
val sum_list: int list -> int
val (--): int -> int -> int list
val set_add_all: ('a, 'b) Base.Set.t -> 'a list -> ('a, 'b) Base.Set.t
val string_to_int_list: ?on:char -> string -> int list
val crt_inductive: (int * int) list -> int

module IntPair : sig
  type t = int * int

  val compare : t -> t -> int
  val sexp_of_t : t -> Base.Sexp.t
  val hash : 'a -> int
end
module IntPair_Comparator : sig
  type t = int * int

  val compare : t -> t -> int
  val sexp_of_t : t -> Base.Sexp.t
  val hash : 'a -> int

  type comparator_witness =
    Base.Comparator.Make(IntPair).comparator_witness

  val comparator : (IntPair.t, comparator_witness) Base.Comparator.t
end

type grid = { cells : char array array; bounds : int * int }
val to_grid: string list -> grid
val grid_fold: grid -> f:(int * int -> 'a -> char -> 'a) -> init:'a -> 'a
val grid_at: grid -> int * int -> char
val grid_cardinal_neighbors: grid -> int * int -> (int * int) list
val grid_diagonal_neighbors: grid -> int * int -> (int * int) list
val grid_all_coords : grid -> (int * int) list
val grid_find : grid -> f:(char -> bool) -> (int * int) option

val num_of_char : char -> int

type tuple = int * int
val equal_tuple: tuple -> tuple -> bool
val show_tuple: tuple -> string
val tuple_range: tuple -> tuple -> tuple list
val isqrt : int -> int
