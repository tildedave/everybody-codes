module type State = sig
  type t [@@deriving compare, sexp_of, hash]
end

val dijkstra :
  (module State with type t = 'a) ->
  'a ->
  is_goal:(int * 'a -> bool) ->
  neighbors:('a -> 'a Base.Sequence.t) ->
  should_cutoff:(int * 'a -> bool) ->
  int option * 'a option
