(** Parser of JSON representation of maps

    This module is used to build pacman mazes*)

(** [build_level w h file_name] is the maze of width [w] and height [h]
    with objects at locations specified in the file [file_name]. If any
    errors arise from parsing the file such as incorrest maze width and
    height or incorrect file formatting, raise [Failure]*)
val build_level : int -> int -> string -> Maze.t
