(** Representation of a pacman.

    Pacman represents a pacman entity (the player) and its associated game data
    such as position and direction *)

open Util
open Components

(** The abstract type of values representing a pacman. *)
type t

(** [create coords dir] is a pacman at coordinates [coords] facing direction
    [dir]. Requires: [coords] contains a valid x and y coordinate. *)
val create : int * int -> Direction.dir -> t

(** [get_coords p] is pacman [p]'s current position (x,y)*)
val get_coords : t -> int * int

(** [facing_dir p] is pacman [p]'s current heading direction *)
val facing_dir : t -> Direction.dir

(** [draw p] draws a pacman [p] using it's coordinate and direction. *)
val draw : t -> unit

(** [move p] is pacman [p] moved in its current heading direction with a speed
    determined by [p]'s speed iff pacman (will) remain in a non-wall tile.
    Otherwise, pacman remains still.*)
val move : Maze.t -> t -> t

(** [change_dir maze p dir] is pacman [p] facing new current direction [dir] and
    has no alternative direction if the next tile in direction [dir] is not a
    wall in [maze]. If there is an immediate wall, [change_dir p dir] is pacman
    [p] with direction [dir] as the alternative direction with current direction
    unchanged*)
val change_dir : Maze.t -> t -> Direction.dir -> t
