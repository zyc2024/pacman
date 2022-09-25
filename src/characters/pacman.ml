open Graphics
open Components
open Util

(** AF : a pacman with position (x,y) with direction d1 and alternative
    direction d2 represents a pacman player on the game map whose
    location is (x,y) who is moving in direction d1 and attempts to move
    in direction d2 if needed. RI : a pacman's current position (x,y)
    must be within the map and not overlap any walls*)
type t = {
  pos : int * int;
  cdir : Direction.dir;
  adir : Direction.dir option;
}

(** The speed at which ghosts travel by defauly. *)
let speed = 2

let create coords dir = { pos = coords; cdir = dir; adir = None }

let get_coords p = p.pos

let facing_dir p = p.cdir

(** [draw_pacman (x,y) r a1 a2] draws a pacman with a radius [r] whose
    center (x,y) is shifted by the given radius [r] in both dimensions.
    The mouth opens at angle [a1] and ends at angle [a2]. Requires:
    x,y,r are positive numbers*)
let draw_pacman (x, y) r a1 a2 =
  let cx = x + r in
  let cy = Graphics.size_y () - (y + r) in
  set_color yellow;
  fill_circle cx cy r;
  (* draw with background color*)
  set_color black;
  fill_arc cx cy r r a1 a2
  [@@coverage off]

let draw (p : t) =
  let pos = get_coords p in
  match p.cdir with
  | North -> draw_pacman pos 8 63 117
  | West -> draw_pacman pos 8 153 208
  | South -> draw_pacman pos 8 243 298
  | East -> draw_pacman pos 8 ~-27 27
  [@@coverage off]

let move maze p =
  let cur_pos = get_coords p in
  let move_with_cdir =
    { p with pos = Maze.entity_move maze cur_pos p.cdir speed }
  in
  match p.adir with
  | Some dir ->
      let alt_pos = Maze.entity_move maze cur_pos dir speed in
      if alt_pos <> cur_pos then
        { pos = alt_pos; cdir = dir; adir = None }
      else move_with_cdir
  | None -> move_with_cdir

let change_dir (m : Maze.t) (p : t) (dir : Direction.dir) =
  if Maze.entity_move m p.pos dir speed <> p.pos then
    { p with cdir = dir; adir = None }
  else if dir <> p.cdir then { p with adir = Some dir }
  else p
