(** Ghost Personality algorithms

    This module holds the personalities of each ghost. Each ghost has its method
    to capturing pacman*)

open Components

(** [Behavior] contains the algorithm that a ghost uses to move around the map
    based on its current priorities (chasing, running away, patrolling)*)
module type Behavior = sig
  (** [move g m p] is ghost [g] after making a move on maze [m] with respect to
      its current mode and pacman [p]'s current states such as direction,
      location, and etc.*)
  val move : Ghosts.t -> Maze.t -> Pacman.t -> Ghosts.t
end

(** [BlinkyBehavior] represents Blinky's personality and approach to capturing
    pacman. Blinky follows pacman directly.*)
module BlinkyBehavior : Behavior

(** [PinkyBehavior] represents Pinky's approach to capturing pacman. Pinky
    ambushes pacman by trying to move to the tiles in front of pacman*)
module PinkyBehavior : Behavior

(** [ClydeBehavior] represents Clyde's unpredictable approach to assist the
    other [Ghosts] in capturing pacman*)
module ClydeBehavior : Behavior

(** [InkyBehavior] represents Inky's approach to capturing pacman. Inky assists
    Blinky in sandwiching pacman. *)
module InkyBehavior : sig
  (** [move g b m p] is Inky [g] after making a move on maze [m] with respect to
      its current mode, pacman [p]'s position, and ghost Blinky [b]'s position.*)
  val move : Ghosts.t -> Ghosts.t -> Maze.t -> Pacman.t -> Ghosts.t
end
