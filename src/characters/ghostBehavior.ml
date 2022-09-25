open Util
open Components
open Ghosts
open Direction
open Maze

module type Behavior = sig
  val move : Ghosts.t -> Components.Maze.t -> Pacman.t -> Ghosts.t
end

(** [dir_cmp m (x1,y1) (x2,y2) dir1 dir2] compares the distances between the
    tile at [(x2,y2)] and each of the adjacent tiles in directions [dir1] and
    [dir2] relative to [(x1,y1)] in maze [m]*)
let dir_cmp m (x1, y1) (x2, y2) dir1 dir2 =
  let dist dir = distance m (neighbor_by_dir m (x1, y1) dir) (x2, y2) in
  let d1 = dist dir1 in
  let d2 = dist dir2 in
  compare d1 d2

(** [dir_rev_cmp m (x1, y1) (x2,y2)] is the reverse comparator (i.e. opposite of
    [dir_cmp]). *)
let dir_rev_cmp m (x1, y1) (x2, y2) dir1 dir2 =
  dir_cmp m (x1, y1) (x2, y2) dir2 dir1

(** [filter_dirs rev_dir m (x,y) dir_lst] is a subset of the list of directions
    [dir_lst] such that the tile in these direction(s) relative to position
    [(x,y)] is not occupied by a wall. None of the directions in the resulting
    set equals to direction [rev_dir]*)
let filter_dirs rev_dir m (x, y) dir_lst =
  List.filter
    (fun d ->
      d <> rev_dir && not (has_struct m Wall (neighbor_by_dir m (x, y) d)))
    dir_lst

(** [best_local_dir cmp get_pref g m target] is the preferred direction (sorted
    by cmp) based on function [get_pref] for a ghost [g] to move relative to its
    [target] on maze [m]. In the case that none of the preferred directions are
    valid, the remaining cardinal directions are sorted by [cmp].*)
let best_local_dir cmp get_pref g m (target : int * int) =
  let gtile = get_tile_pos m (get_coords g) in
  let prefered = get_pref m gtile target in
  let others = Direction.complement prefered in
  let disallowed = opposite (get_dir g) in
  let filter = filter_dirs disallowed m gtile in
  let comparator = cmp m gtile target in
  match filter prefered |> List.sort comparator with
  | [] ->
      let alt_dirs = filter others |> List.sort comparator in
      List.hd alt_dirs
      (* there is no dead so there is always a direction to exit*)
  | h :: _ -> h

(** [frightened_dir g m p] is the preferred (allowed) direction for ghost [g] to
    travel to maintain the greatest distance from pacman [p] in maze [m]*)
let frightened_dir g m p =
  let target = get_tile_pos m (Pacman.get_coords p) in
  let dirs_away m g p = Maze.travel_dirs m g p |> Direction.complement in
  best_local_dir dir_rev_cmp dirs_away g m target

(** The speed at which a ghost moves by default. *)
let ghost_speed = 2

(** The width and height of each tile. *)
let tile_size = 16

(* [generic_move chase_algo scatter_point g m p] is a generic move-by-mode
   algorithm for ghosts. Considers the three modes of Ghosts: chase, frightened,
   scatter.*)
let rec generic_move chase_algo scatter_point g m p =
  let mode = Ghosts.get_mode g in
  let jail_exit = Maze.get_jail_coords m in
  match mode with
  | Frightened ->
      let dir = frightened_dir g m p in
      move m g dir (ghost_speed / 2)
  | Chase -> chase_algo g m p
  | Scatter ->
      let dir = best_local_dir dir_cmp travel_dirs g m scatter_point in
      move m g dir ghost_speed
  | Dead ->
      if Ghosts.get_coords g = jail_exit then
        generic_move chase_algo scatter_point (set_mode Chase g) m p
      else
        let dir = best_local_dir dir_cmp travel_dirs g m jail_exit in
        move m g dir (ghost_speed * 2)

(** [pick_target g m desired] is [desired] when ghost [g] is not in the jail in
    maze [m]. Otherwise, return position of the jail exit in [m].*)
let pick_target g m desired =
  if Ghosts.is_in_jail g then Maze.get_jail_coords m else desired

module BlinkyBehavior = struct
  let aggressive (g : Ghosts.t) (m : Maze.t) (p : Pacman.t) =
    let target = pick_target g m (get_tile_pos m (Pacman.get_coords p)) in
    let dir = best_local_dir dir_cmp travel_dirs g m target in
    Ghosts.move m g dir ghost_speed

  let move g m p =
    let scatter = pick_target g m ((Maze.width m - 1) * tile_size, 0) in
    generic_move aggressive scatter g m p
end

module PinkyBehavior = struct
  let ambush (g : Ghosts.t) (m : Maze.t) (p : Pacman.t) =
    let ptile = get_tile_pos m (Pacman.get_coords p) in
    let pdir = Pacman.facing_dir p in
    let rec loop acc rep pos =
      if acc >= rep then pos
      else loop (acc + 1) rep (neighbor_by_dir m pos pdir)
    in
    let target = pick_target g m (loop 0 3 ptile) in
    let dir = best_local_dir dir_cmp travel_dirs g m target in
    Ghosts.move m g dir ghost_speed

  let move g m p =
    let scatter = pick_target g m (0, 0) in
    generic_move ambush scatter g m p
end

module ClydeBehavior = struct
  let distancing (g : Ghosts.t) (m : Maze.t) (p : Pacman.t) =
    let pcoords = Pacman.get_coords p in
    let gcoords = Ghosts.get_coords g in
    let target = pick_target g m (get_tile_pos m pcoords) in
    if Maze.distance m pcoords gcoords > float_of_int (8 * tile_size) then
      let dir = best_local_dir dir_cmp travel_dirs g m target in
      Ghosts.move m g dir ghost_speed
    else
      let dir = frightened_dir g m p in
      Ghosts.move m g dir ghost_speed

  let move g m p =
    let scatter = pick_target g m (0, (Maze.height m - 1) * tile_size) in
    generic_move distancing scatter g m p
end

module InkyBehavior = struct
  let vector_target m (x1, y1) (x2, y2) =
    Maze.normalize m ((2 * (x2 - x1)) + x1, (2 * (y2 - y1)) + y1)

  let assist blinky g m p =
    let desired =
      vector_target m (get_coords blinky) (get_tile_pos m (Pacman.get_coords p))
    in
    let target = pick_target g m desired in
    let dir = best_local_dir dir_cmp travel_dirs g m target in
    Ghosts.move m g dir ghost_speed

  let move g blinky m p =
    let scatter =
      pick_target g m
        ((Maze.width m - 1) * tile_size, (Maze.height m - 1) * tile_size)
    in
    generic_move (assist blinky) scatter g m p
end
