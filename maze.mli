(** Representation of a maze

    A maze represents the gameboard or map that entities (pacman and
    ghosts) travel around on. The maze is built with a collection of
    walls and consumable items*)

(** a [fruit] is one of the consumable items in the game which rewards
    more points than dots.*)
type fruit =
  | Cherry
  | Strawberry
  | Orange
  | Apple
  | Melon

(** a [structure] represents one of the possible types of tiles on the
    gameboard*)
type structure =
  | Dot
  | PowerDot
  | Path
  | Wall
  | JailExit
  | Fruit of fruit

(** The abstract type representing the entire gameboard map. *)
type t

(** [grid_init w h] is a maze with width [w] and height [h] whose tiles
    have size 16. Requires [w] and [h] are greater than 0*)
val grid_init : int -> int -> t

(** [width m] is the width of the maze or equivalently the number of
    tile columns in maze [m]*)
val width : t -> int

(** [height m] is the height of the maze or equivalently the number of
    tile rows in maze [m]*)
val height : t -> int

(** [normalize m (x,y)] is [(x,y)] normalized within the boundaries of
    maze [m]. Example: in a 400px by 500px maze with x coordinate range
    [\[0,399\]] and y coordinate range [\[0,499\]], coordinate [(-1,0)]
    corresponds to [(399,0)]*)
val normalize : t -> int * int -> int * int

(** [make_struct m s (x,y)] constructs a structure [s] in maze [m] at
    the tile corresponding to position [(x,y)].*)
val make_struct : t -> structure -> int * int -> unit

(** [has_struct m s (x,y)] is true iff tile at position [(x,y)] in maze
    [m] has structure [s].*)
val has_struct : t -> structure -> int * int -> bool

(** [count_struct m s] is the number of tiles that are occupied by
    structure [s]. Example: if there is exactly 1 [Dot] somewhere on
    [m], [count_struct m Dot] is 1*)
val count_struct : t -> structure -> int

(** [has_some_fruit m (x,y)] is true iff there exists a fruit of any
    kind at tile corresponding to [(x,y)].*)
val has_some_fruit : t -> int * int -> bool

(** [fruit_points m (x,y)] is the point value worth of the uncollected
    fruit at tile corresponding to position [(x,y)] in maze [m].
    Requires : there exists a fruit at position [(x,y)].*)
val fruit_points : t -> int * int -> int

(** [neighbor_by_dir m (x,y) dir] is the adjacent tile's position
    [(x2,y2)] in the direction [dir] of the tile at position [(x,y)] in
    maze [m]*)
val neighbor_by_dir : t -> int * int -> Direction.dir -> int * int

(** [get_neighbors m (x,y)] is the list of the tile positions
    surrounding the tile in maze [m] at position [(x,y)]. Tiles on the
    border of the map may have less than 4 neighboring tiles*)
val get_neighbors : t -> int * int -> (int * int) list

(** [draw_maze m] draws every tile on maze [m]*)
val draw_maze : t -> unit

(** [get_tile_pos m (x,y)] is position [(x2,y2)] which represents the
    tile that [(x,y)] lies on. Example: [get_tile_pos m (0,2)] and
    [get_tile_pos m (2,2)] are both [(0,0)] on a maze whose tiles have a
    size of 16*)
val get_tile_pos : t -> int * int -> int * int

(** [moved_pos m (x, y) d dir] is the new position in [m] after moving a
    horizontal distance [d] in direction [dir] away from [(x,y)]. Note
    this position can be located on any type of tile.*)
val moved_pos : t -> int * int -> int -> Direction.dir -> int * int

(** [entity_move m ?jail_ok (x,y) dir d] is the new position [(x2,y2)]
    after moving from position [(x,y)] by a distance [d] in direction
    [dir] in maze [m]. If moving in such manner results in a position
    that violates collision with walls (including jails), no change in
    position will occur. [?jail_ok] should be set to [true] to ignore
    any [JailExit] from preventing such move. Requires : [(x,y)] is not
    on a [Wall] in maze [m] *)
val entity_move :
  t -> ?jail_ok:bool -> int * int -> Direction.dir -> int -> int * int

(** [distance m (x1,y1) (x2, y2)] is the distance between the tile that
    contains [(x1,y1)] and the tile that contains [(x2,y2)]. If
    [(x1,y1)] and [(x2,y2)] are in the same tile, the distance is 0. *)
val distance : t -> int * int -> int * int -> float

(** [travel_dirs m (x1,y1) (x2,y2)] is the list of directions that when
    headed towards will bring position [(x1,y1)] in maze [m] closer to
    position [(x2,y2)]. An empty list of directions suggests that
    [(x1,y1)] and [(x2,y2)] are equal. The list of directions is sorted
    relative to the order of [\[North;West;South;East\]].*)
val travel_dirs : t -> int * int -> int * int -> Direction.dir list

(** [get_jail_coords m] is the coordinate pair representing the location
    of the most recently constructed jail exit in [m]. Requires: maze
    [m] has exactly two adjacent tiles for a valid ghost jail exit.*)
val get_jail_coords : t -> int * int
