(** Visual representation of Tiles

    This module is used to draw the different tiles on a pacman maze *)

(** [draw_path coords] draws an empty tile with hitbox starting from
    [coords]*)
val draw_path : int * int -> unit

(** [draw_jail_exit coords] draws the opening of the ghost jail with
    hitbox starting from [coords]*)
val draw_jail_exit : int * int -> unit

(** [draw_wall coords] draws a wall with hitbox starting from [coords] *)
val draw_wall : int * int -> unit

(** [draw_dot coords is_big] draws a power-up dot iff [is_big],
    otherwise it draws a regular dot (coin) with hitbox starting from
    [coords]*)
val draw_dot : int * int -> bool -> unit

(** [draw_cherry coords] draws a cherry with hitbox starting from
    [coords]*)
val draw_cherry : int * int -> unit

(** [draw_berry coords] draws a strawberry with hitbox starting from
    [coords]*)
val draw_berry : int * int -> unit

(** [draw_orange coords] draws an orange with hitbox starting from
    [coords]*)
val draw_orange : int * int -> unit

(** [draw_apple coords] draws an apple with hitbox starting from
    [coords]*)
val draw_apple : int * int -> unit

(** [draw_melon coords] draws a melon with hitbox starting from [coords]*)
val draw_melon : int * int -> unit
