(*
 * David Méndez
 * hello@davidemdot.com
 *)

exception Knight_tour_solution of (int * int) list

module IntPair =
  struct
    type t = int * int
    let compare (x0, y0) (x1, y1) =
      match Pervasives.compare x0 x1 with
      | 0 -> Pervasives.compare y0 y1
      | c -> c
  end

module S = Set.Make(IntPair)


(* Get a sequence of moves of a knight between a couple of squares on a
   chessboard (m * n), such that the knight visits every square only once.
   Example of use: knight_tour 20 20 (1, 1) (14, 17) *)

let search_dfs m n startxy endxy =
  let is_valid (x, y) visited = x > 0 && x <= m &&
                                y > 0 && y <= n &&
                                (x, y) <> endxy &&
                                not (S.mem (x, y) visited) in
  let offset = [(1, 2); (1, -2); (-1, 2); (-1, -2);
                (2, 1); (2, -1); (-2, 1); (-2, -1)] in
  let add (x, y) (dx, dy) = (x + dx, y + dy) in
  let warnsdorff (x, y) visited = List.fold_left (fun n (dx, dy) ->
      if is_valid (x + dx, y + dy) visited
      then n + 1
      else n) 0 offset in
  let rec loop hops ((x, y) as xy) path visited =
    if hops >= m * n
    then []
    else if hops = m * n - 1 &&
            List.exists (fun off -> add xy off = endxy) offset
    then raise (Knight_tour_solution (List.rev (endxy :: xy :: path)))
    else let moves = List.fold_left (fun lst off ->
        let newxy = add xy off in
        if is_valid newxy visited
        then (warnsdorff newxy visited, newxy) :: lst
        else lst) [] offset in
    List.iter (fun (_, nextxy) ->
        ignore (loop (hops + 1) nextxy (xy :: path) (S.add xy visited)))
        (List.sort (fun (n1, _) (n2, _) -> n1 - n2) moves);
    [] in
  loop 1 startxy [] (S.empty)

let knight_tour m n startxy endxy =
  let check (x, y) = x > 0 && x <= m && y > 0 && y <= n in
  if m < 1 || n < 1 || not (check startxy && check endxy)
  then raise (Invalid_argument "knight_tour")
  else
    try
      if search_dfs m n startxy endxy = []
      then raise Not_found; []
    with
      Knight_tour_solution s -> s


(* Look for the shortest path of a knight between a couple of squares on a
   chessboard (m * n).
   Example of use: shortest_knight_tour 1000 1000 (1, 1) (100, 100) *)

let visited = ref S.empty

let check m n (x, y) = x > 0 && x <= m &&
                       y > 0 && y <= n &&
                       not (S.mem (x, y) !visited)

let next_moves m n (x, y) =
  let offset = [(1, 2); (1, -2); (-1, 2); (-1, -2);
                (2, 1); (2, -1); (-2, 1); (-2, -1)] in
  List.filter (fun xy -> check m n xy)
      (List.map (fun (dx, dy) -> (x + dx, y + dy)) offset)

let rec search_bfs m n startxy endxy q =
  if not (Queue.is_empty q)
  then
    let (x, y, path) = Queue.pop q in
    if (x, y) = endxy
    then raise (Knight_tour_solution (List.rev ((x, y) :: path)))
    else List.iter (fun (nextx, nexty) ->
        Queue.add (nextx, nexty, (x, y) :: path) q;
        visited := S.add (nextx, nexty) !visited) (next_moves m n (x, y))
  else raise Not_found;
  search_bfs m n startxy endxy q

let shortest_knight_tour m n startxy endxy =
  visited := S.empty;
  if m < 1 || n < 1 || not (check m n startxy && check m n endxy)
  then raise (Invalid_argument "shortest_knight_tour")
  else begin
    let q = Queue.create () in
    Queue.add (fst startxy, snd startxy, []) q;
    visited := S.add startxy !visited;
    try
      let _ = search_bfs m n startxy endxy q in []
    with
      Knight_tour_solution s -> s
  end
