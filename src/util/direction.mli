(** Cardinal Directions*)

(** cardinal constants*)
type dir =
  | North
  | East
  | South
  | West

(** an ordered-set-like list of the cardinal directions in order of
    [\[N;W;S;E\]]*)
val dir_list : dir list

(** [opposite dir] is the direction opposite of [dir]*)
val opposite : dir -> dir

(** [complement dlst] is an ordered-set-like list of cardinal directions
    that complements [dlst] which together forms [dir_list]. The
    complement of [dir_list] is the empty list.*)
val complement : dir list -> dir list
