open Util
open Draw

type fruit =
  | Cherry
  | Strawberry
  | Orange
  | Apple
  | Melon

(** A wall is either an actual wall or the exit to the jail. *)
type walls =
  | MapWall
  | Exit

type structure =
  | Dot
  | PowerDot
  | Path
  | Wall
  | JailExit
  | Fruit of fruit

let tile_size = 16

(* A point tile holds coordinates and the type of pellet at that
   position*)
type point_tile = {
  coords : int * int;
  big : bool;
}

(* A fruit tile holds coordinates and the type of fruit at that position*)
type fruit_tile = {
  coords : int * int;
  fruit : fruit;
}

(* A wall tile holds coordinates and the type of wall at that position*)
type wall_tile = {
  coords : int * int;
  wtype : walls;
}

(** a [tile] represents one individual square unit of area on the map *)
type tile =
  | WallTile of wall_tile
  | PathTile of { coords : int * int }
  | PointTile of point_tile
  | FruitTile of fruit_tile

(** AF : a maze (alternatively, a game map) with n rows of tiles
    [t1...tm] represents the map whose dimensions are n and m with tiles
    [\[t11...t1m\]...\[tn1...tnm\]]. RI : the map must be rectangular
    and have tile data associated with every row and column*)
type t = {
  map : tile array array;
  exit : (int * int) list ref;
}

let width (m : t) = Array.length m.map

let height (m : t) = Array.length m.map.(0)

(** [draw_fruit ft] draws the corresponding fruit at the tile's position*)
let draw_fruit (ft : fruit_tile) =
  match ft.fruit with
  | Cherry -> TileDraw.draw_cherry ft.coords
  | Strawberry -> TileDraw.draw_berry ft.coords
  | Orange -> TileDraw.draw_orange ft.coords
  | Apple -> TileDraw.draw_apple ft.coords
  | Melon -> TileDraw.draw_melon ft.coords
  [@@coverage off]

(** [draw_walls wall] draws either an exit or a solid wall block*)
let draw_walls (wt : wall_tile) =
  match wt.wtype with
  | MapWall -> TileDraw.draw_wall wt.coords
  | Exit -> TileDraw.draw_jail_exit wt.coords
  [@@coverage off]

(** [tile_draw tile] draws a the structure on the [tile] *)
let tile_draw tile =
  match tile with
  | WallTile t -> draw_walls t
  | PointTile t -> TileDraw.draw_dot t.coords t.big
  | PathTile t -> TileDraw.draw_path t.coords
  | FruitTile t ->
      TileDraw.draw_path t.coords;
      draw_fruit t
  [@@coverage off]

let grid_init w h =
  let m =
    Array.init w (fun i ->
        Array.init h (fun j ->
            PointTile
              { coords = (i * tile_size, j * tile_size); big = false }))
  in
  { map = m; exit = ref [] }

let normalize (maze : t) (x, y) =
  let pmod n m =
    let r = n mod m in
    if r < 0 then r + m else r
  in
  let nx = pmod x (16 * width maze) in
  let ny = pmod y (16 * height maze) in
  (nx, ny)

(** [get_index m (x,y)] is the index pair (i,j) of the tile that
    position [(x,y)] lies on in maze [m]*)
let get_index m (x, y) =
  let size_f = tile_size |> float_of_int in
  let i, j =
    ( (float_of_int x /. size_f) +. 0.49 |> floor |> int_of_float,
      (float_of_int y /. size_f) +. 0.49 |> floor |> int_of_float )
  in
  (i mod width m, j mod height m)

(** [get_tile m (x,y)] is the tile at position [(x,y)] in maze [m]*)
let get_tile (m : t) (x, y) =
  let index = get_index m (x, y) in
  let i = fst index in
  let j = snd index in
  m.map.(i).(j)

let get_tile_pos m (x, y) =
  match get_tile m (x, y) with
  | WallTile wt -> wt.coords
  | PointTile pd -> pd.coords
  | PathTile pt -> pt.coords
  | FruitTile ft -> ft.coords

let distance (m : t) (x1, y1) (x2, y2) =
  let x1, y1 = get_tile_pos m (x1, y1) in
  let x2, y2 = get_tile_pos m (x2, y2) in
  let dx = x1 - x2 |> abs |> float_of_int in
  let dy = y1 - y2 |> abs |> float_of_int in
  ((dx ** 2.) +. (dy ** 2.)) ** 0.5

let make_struct (m : t) s (x, y) =
  let i, j = get_index m (x, y) in
  let coords = get_tile_pos m (x, y) in
  let map = m.map in
  (* an existing jail occupying the current tile needs to be removed
     from list of exits.*)
  m.exit := List.filter (fun pos -> pos <> coords) !(m.exit);
  match s with
  | Dot -> map.(i).(j) <- PointTile { coords; big = false }
  | PowerDot -> map.(i).(j) <- PointTile { coords; big = true }
  | Path -> map.(i).(j) <- PathTile { coords }
  | Wall -> map.(i).(j) <- WallTile { coords; wtype = MapWall }
  | JailExit ->
      map.(i).(j) <- WallTile { coords; wtype = Exit };
      m.exit := coords :: !(m.exit)
  | Fruit f -> map.(i).(j) <- FruitTile { coords; fruit = f }

let has_struct m s (x, y) =
  let eq_walltype wt =
    match wt with MapWall -> Wall = s | Exit -> JailExit = s
  in
  let eq_fruits f = match s with Fruit f2 -> f = f2 | _ -> false in
  match get_tile m (x, y) with
  | WallTile t -> eq_walltype t.wtype
  | PointTile t -> if t.big then PowerDot = s else Dot = s
  | PathTile _ -> Path = s
  | FruitTile t -> eq_fruits t.fruit

let has_some_fruit m (x, y) =
  let has_fruit_type ft = has_struct m ft (x, y) in
  has_fruit_type (Fruit Cherry)
  || has_fruit_type (Fruit Strawberry)
  || has_fruit_type (Fruit Orange)
  || has_fruit_type (Fruit Apple)
  || has_fruit_type (Fruit Melon)

let count_struct m s =
  let count = ref 0 in
  Array.iteri
    (fun i arr ->
      Array.iteri
        (fun j _ ->
          if has_struct m s (i * 16, j * 16) then count := !count + 1)
        arr)
    m.map;
  !count

let fruit_points m (x, y) =
  let my_tile = get_tile m (x, y) in
  match my_tile with
  | FruitTile t -> (
      match t.fruit with
      | Cherry -> 100
      | Strawberry -> 300
      | Orange -> 500
      | Apple -> 700
      | Melon -> 1000)
  | _ -> raise (Failure "precondition violated")

let neighbor_by_dir m (x, y) (dir : Direction.dir) =
  let i, j = (x, y) |> get_index m in
  (match dir with
  | North -> (i * tile_size, (j - 1) * tile_size)
  | West -> ((i - 1) * tile_size, j * tile_size)
  | South -> (i * tile_size, (j + 1) * tile_size)
  | East -> ((i + 1) * tile_size, j * tile_size))
  |> normalize m

let get_neighbors (maze : t) ((x : int), (y : int)) =
  List.map
    (fun dir -> neighbor_by_dir maze (x, y) dir)
    Direction.dir_list

let draw_maze (m : t) =
  Array.iter (fun arr -> Array.iter (fun e -> tile_draw e) arr) m.map
  [@@coverage off]

open Direction

let moved_pos m (x, y) d dir =
  (match dir with
  | North -> (x, y - d)
  | West -> (x - d, y)
  | South -> (x, y + d)
  | East -> (x + d, y))
  |> normalize m

let entity_move m ?(jail_ok = false) (x, y) (dir : Direction.dir) d =
  let check_y = y mod tile_size = 0 in
  let check_x = x mod tile_size = 0 in
  let x2, y2 = moved_pos m (x, y) d dir in
  let final_new_pos pos_condition pos =
    if
      pos_condition
      && (not (has_struct m Wall pos))
      && (jail_ok || dir = North || not (has_struct m JailExit pos))
    then (x2, y2)
    else (x, y)
  in
  let shift = tile_size / 2 in
  match dir with
  | North -> final_new_pos check_x (x2, y2 - shift + 1)
  | West -> final_new_pos check_y (x2 - shift + 1, y2)
  | South -> final_new_pos check_x (x2, y2 + shift)
  | East -> final_new_pos check_y (x2 + shift, y2)

let travel_dirs m (x1, y1) (x2, y2) =
  let x1, y1 = normalize m (x1, y1) in
  let x2, y2 = normalize m (x2, y2) in
  (* preference to either [d1] or [d2] or none depending on comparison
     result [r] of positions*)
  let get_prefer_dir r d1 d2 =
    match r with c when c < 0 -> [ d1 ] | 0 -> [] | _ -> [ d2 ]
  in
  let ydir = get_prefer_dir (compare y1 y2) South North in
  let xdir = get_prefer_dir (compare x1 x2) East West in
  (* maintain direction order preference such that N > W > S > E*)
  match ydir @ xdir with
  | [ South; West ] -> [ West; South ]
  | lst -> lst

let get_jail_coords (m : t) =
  let jails = !(m.exit) in
  match jails with
  | [ (x, y); (x', _) ] when abs (x - x') = 16 -> (x, y)
  | _ -> failwith "invalid map"
