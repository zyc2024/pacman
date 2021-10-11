type dir =
  | North
  | East
  | South
  | West

let dir_list = [ North; West; South; East ]

let opposite dir =
  match dir with
  | North -> South
  | East -> West
  | South -> North
  | West -> East

let complement dlst =
  List.filter (fun d -> not (List.mem d dlst)) dir_list
