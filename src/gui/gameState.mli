(** Representation of a GameState.

    This module represents the data stored about a game, including the ghosts,
    pacman, maze, lives, score, and more. *)

(** The abstract type of values representing a game. *)
type t

(** [init_game w h score lives] initializes a game that has a maze of dimensions
    [w] x [h] with 4 ghosts, and a pacman with number of [lives], with current
    [score]. *)
val init_game : int -> int -> int -> int -> t

(** [init_display game] initializes the graphics window for [game]. *)
val init_display : t -> unit

(** [get_score game] is the current score of [game]. *)
val get_score : t -> int

(** [game_won game] is true iff the player has won the [game]. *)
val game_won : t -> bool

(** [is_alive game] is true iff a player is currently alive in [game] (i.e. the
    ghosts haven't caught pacman). *)
val is_alive : t -> bool

(** [num_lives game] is the number of lives the player has left in [game]. *)
val num_lives : t -> int

(** [draw_screen game] updates the graphical window with the current location of
    pacman, the ghosts, all points, and any other things that are drawn on
    screen in [game]. *)
val draw_screen : t -> int -> unit

(** [user_input game] changes [game] iff one of space, w, a, s, or d are
    pressed. *)
val user_input : t -> t

(** [move game] moves pacman and ghosts in [game]. *)
val move : t -> t

(** [eat game] updates [game] so that pacman eats the dot at its current
    location. *)
val eat : t -> t

(** [collision game] changes [game] iff one or more ghosts catches pacman. *)
val collision : t -> t

(** [reinit game] reinitializes [game] after the ghosts catch pacman. *)
val reinit : t -> t

(** [update_ghost_jail_state game] updates the ghosts in [game] to have the
    proper jail state. *)
val update_ghost_jail_state : t -> t

(** [update_ghost_mode level game] updates the ghosts in [game] to have the
    proper movement mode based on the current [level]. *)
val update_ghost_mode : int -> t -> t

(** [update_fruit game] updates which fruit is displayed in [game]. *)
val update_fruit : t -> t

(** [paused game] is true iff [game] is currently paused. *)
val paused : t -> bool

(** [restart] returns true iff the player presses the restart key at the end of
    the game. *)
val restart : unit -> bool
