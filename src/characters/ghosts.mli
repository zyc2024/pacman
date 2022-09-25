(** Representation of a ghost.

    This module represents the data stored about a ghost, including the
    coordinates, name, and color. *)

open Util
open Components

(** a [name] represents the identity of a ghost*)
type name =
  | Blinky
  | Pinky
  | Inky
  | Clyde

(** a [mode] represents the mental state of a ghost in the game. A ghost with
    [Chase] mode is actively pursuing [Pacman]*)
type mode =
  | Chase
  | Scatter
  | Frightened
  | Dead

(** The abstract type of values representing a ghost. *)
type t

(** [create name loc dir] creates a ghost with [name] at [loc] facing [dir].
    Each ghost has its own personality, movement techniques, and color.
    Requires: [loc] contains a valid x and y coordinate. *)
val create : name -> int * int -> Direction.dir -> t

(** [draw ghost] draws a ghost using it's coordinate information stored in
    [ghost]. Requires: a call to clear_graph () before drawing. Otherwise, the
    output will not look proper. *)
val draw : t -> unit

(** [move m ghost dir mvmt_speed] tries to move [ghost] in [dir] at
    [mvmt_speed]. If making such move results in colliding with a wall in maze
    [m], [ghost] tries to move in its most-direct direction.*)
val move : Maze.t -> t -> Direction.dir -> int -> t

(** [get_dir ghost] is [ghost]'s current heading direction *)
val get_dir : t -> Direction.dir

(** [get_coords ghost] is [ghost]'s x and y coordinates. *)
val get_coords : t -> int * int

(** [set_mode mode ghost] is [ghost] with mental mode switched to [mode]. *)
val set_mode : mode -> t -> t

(** [get_mode ghost] is [ghost]'s current mode*)
val get_mode : t -> mode

(** [set_in_jail jailed ghost] is [ghost] with in_jail status switched to
    [jailed]*)
val set_in_jail : bool -> t -> t

(** [is_in_jail ghost] is true iff [ghost] is in the jail/ghost pen, otherwise
    false. *)
val is_in_jail : t -> bool

(** [collision ghost pacman m] is true if [ghost] and [pacman] collide in [m],
    false otherwise. *)
val collision : t -> Pacman.t -> Maze.t -> bool

(** [update_jail_status g m] changes [g]'s in_jail status to true if the dead
    ghost has returned to the jail in maze [m] and to false if the ghost is
    leaving that jail. Otherwise, the in_jail status remains unchanged.*)
val update_jail_status : t -> Maze.t -> t
