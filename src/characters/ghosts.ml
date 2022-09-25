open Util
open Components

type mode =
  | Chase
  | Scatter
  | Frightened
  | Dead

type name =
  | Blinky
  | Pinky
  | Inky
  | Clyde

type t = {
  color : Graphics.color;
  pos : int * int;
  cdir : Direction.dir;
  mode : mode;
  in_jail : bool;
}

let create name loc dir =
  let template =
    {
      color = Graphics.rgb 0 0 0;
      pos = loc;
      cdir = dir;
      mode = Scatter;
      in_jail = true;
    }
  in
  match name with
  | Blinky -> { template with color = Graphics.rgb 255 0 0 }
  | Pinky -> { template with color = Graphics.rgb 255 105 180 }
  | Inky -> { template with color = Graphics.rgb 0 255 255 }
  | Clyde -> { template with color = Graphics.rgb 255 165 0 }

let set_mode mode ghost = { ghost with mode }

let get_mode ghost = ghost.mode

let set_in_jail in_jail ghost = { ghost with in_jail }

let is_in_jail ghost = ghost.in_jail

(** The width and height of each tile. *)
let px = 16

(** [draw_eyes ghost x y] draws [ghost]'s eyes, where the center of
    [ghost] is ([x], [y]). *)
let draw_eyes ghost x y =
  Graphics.(
    set_color white;
    fill_ellipse (x - (px / 6) - 1) (y + (px / 6)) 2 3;
    fill_ellipse (x + (px / 6)) (y + (px / 6)) 2 3;
    set_color black;
    match ghost.cdir with
    | North ->
        fill_circle (x - (px / 6) - 1) (y + (px / 6) + 1) 1;
        fill_circle (x + (px / 6)) (y + (px / 6) + 1) 1
    | South ->
        fill_circle (x - (px / 6) - 1) (y + (px / 6) - 1) 1;
        fill_circle (x + (px / 6)) (y + (px / 6) - 1) 1
    | West ->
        fill_circle (x - (px / 6) - 2) (y + (px / 6)) 1;
        fill_circle (x + (px / 6) - 1) (y + (px / 6)) 1
    | East ->
        fill_circle (x - (px / 6)) (y + (px / 6)) 1;
        fill_circle (x + (px / 6) + 1) (y + (px / 6)) 1)
  [@@coverage off]

let draw ghost =
  let is_frightened = ghost.mode = Frightened in
  let dead = ghost.mode = Dead in
  Graphics.(
    set_color (if is_frightened then blue else ghost.color);
    let x = fst ghost.pos + (px / 2) in
    let y = Graphics.size_y () - px - snd ghost.pos + (px / 2) in
    if not dead then (
      fill_arc x (y - (px / 3)) (px / 2) (5 * px / 6) 0 180;
      fill_ellipse (x - (px / 3)) (y - (px / 3)) 2 4;
      fill_ellipse x (y - (px / 3)) 2 4;
      fill_ellipse (x + (px / 3)) (y - (px / 3)) 2 4;
      if not is_frightened then draw_eyes ghost x y
      else (
        set_color (rgb 222 184 135);
        fill_circle (x - (px / 6)) (y + (px / 6)) 1;
        fill_circle (x + ((px / 6) + 1)) (y + (px / 6)) 1;
        fill_circle x (y - (px / 6)) 2))
    else draw_eyes ghost x y)
  [@@coverage off]

let get_coords ghost = ghost.pos

let get_dir ghost = ghost.cdir

(** [fix_position g m speed] updates [g]'s position if ghost [g] will
    eventually be unable to move at a rate of [speed] due to limitations
    in generic move algorithm below. This problem will likely arise from
    switching between frightened and dead mode.*)
let rec fix_position g m speed =
  let gx, gy = get_coords g in
  match (gx mod speed, gy mod speed) with
  | 0, 0 -> g
  | 0, d | d, _ -> move m g g.cdir (speed - d)

and move maze ghost (dir : Direction.dir) mvmt_speed =
  let ghost = fix_position ghost maze mvmt_speed in
  let x1, y1 = get_coords ghost in
  let x2, y2 =
    match ghost.mode with
    | Dead ->
        Maze.entity_move maze ~jail_ok:true (x1, y1) dir mvmt_speed
    | _ -> Maze.entity_move maze (x1, y1) dir mvmt_speed
  in
  if (x1, y1) <> (x2, y2) then { ghost with cdir = dir; pos = (x2, y2) }
  else
    let move_with_cdir =
      Maze.entity_move maze (x1, y1) (get_dir ghost) mvmt_speed
    in
    { ghost with pos = move_with_cdir }

let collision (ghost : t) (p : Pacman.t) (_m : Maze.t) =
  let gx, gy = get_coords ghost in
  let tx, ty = Pacman.get_coords p in
  let left = max gx tx in
  let right = min (gx + px) (tx + px) in
  let bottom = min (gy + px) (ty + px) in
  let top = max gy ty in
  (left < right && top < bottom)
  && (right - left) * (bottom - top) >= px * px / 4

let update_jail_status (ghost : t) (maze : Maze.t) =
  let x, y = Maze.get_jail_coords maze in
  let gcoords = get_coords ghost in
  (* ghost is coming back to jail (terminating dead mode)*)
  if gcoords = (x, y) && ghost.cdir = South then set_in_jail true ghost
  else if gcoords = (x, y - 16) then set_in_jail false ghost
  else ghost
