open Base

module type State = sig
  type t [@@deriving compare, sexp_of, hash]
end

let dijkstra (type s) (module S : State with type t = s) root ~is_goal
    ~neighbors ~should_cutoff =
  let module Entry = struct
    type t = int * S.t [@@deriving compare, sexp_of]

    include (val Comparator.make ~compare ~sexp_of_t)
  end in
  let dist = Hashtbl.create (module S) in
  let prev = Hashtbl.create (module S) in
  let result = ref None in
  let final = ref None in
  Hashtbl.set dist ~key:root ~data:0;
  let frontier = ref (Set.singleton (module Entry) (0, root)) in
  while (not (Set.is_empty !frontier)) && Option.is_none !result do
    let ((curr_dist, next) as old) = Set.min_elt_exn !frontier in
    frontier := Set.remove !frontier old;
    if not (should_cutoff old) then
      if is_goal (curr_dist, next) then (
        result := Some curr_dist;
        final := Some next)
      else
        Sequence.iter (neighbors next) ~f:(fun neighbor_state ->
            match Hashtbl.find dist neighbor_state with
            | Some d' when d' <= curr_dist + 1 -> ()
            | old ->
                Option.iter
                  ~f:(fun d' ->
                    assert (Set.mem !frontier (d', neighbor_state));
                    frontier := Set.remove !frontier (d', neighbor_state))
                  old;
                frontier := Set.add !frontier (curr_dist + 1, neighbor_state);
                Hashtbl.set prev ~key:neighbor_state ~data:next;
                Hashtbl.set dist ~key:neighbor_state ~data:(curr_dist + 1))
    (* we are done *)
  done;
  (!result, !final)
