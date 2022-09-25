open Graphics

(** The width and height of each tile. *)
let tile_size = 16

(** The radius of a point dot. *)
let point_radius = 1

(** The color of the walls. *)
let wall_color = Graphics.rgb 54 134 183

(** The color of a point dot. *)
let point_filling_color = Graphics.rgb 255 229 180

(* Graphics origin is at the bottom left corner. Adjust by raising initial y
   coordinate to height of Graphics window. This is now the new origin. Example
   : (0,100) would now be 100 pixels downwards from the top left corner. Because
   fill_rect draws from the bottom corner, the y-value is shifted down by
   tile_size to keep the rectangle at a distance from the new origin as
   represented by its coordinate (x,y)*)

(** [draw_solid_tile (x, y) color] draws a [color] tile at [(x, y)]. *)
let draw_solid_tile (x, y) color =
  Graphics.set_color color;
  Graphics.fill_rect x (Graphics.size_y () - y - tile_size) tile_size tile_size

(** [draw_fruit_body] draws a circle whose center is [(adjx, adjy)] and radius
    [r] representing a fruit of [color]. Requires that [adjx] and [adjy]
    represents a coordinate with respect to top left corner of graphics*)
let draw_fruit_body (adjx, adjy) color r =
  set_color color;
  draw_circle adjx adjy r;
  fill_circle adjx adjy r

let draw_cherry (x, y) =
  let y = size_y () - y - 9 in
  let x = x + 5 in
  draw_fruit_body (x, y) red 2;
  draw_circle (x + 4) (y - 2) 2;
  fill_circle (x + 4) (y - 2) 2;
  set_color green;
  draw_poly_line [| (x + 1, y + 2); (x + 5, y + 6) |];
  draw_poly_line [| (x + 5, y); (x + 5, y + 6) |]

let draw_berry (x, y) =
  let y = size_y () - y - 9 in
  let x = x + (tile_size / 2) in
  draw_fruit_body (x, y) red 4;
  set_color green;
  fill_rect x (y + 2) 2 1;
  fill_rect (x - 2) (y + 3) 2 1;
  fill_rect x (y + 4) 1 1;
  set_color white;
  fill_rect (x + 2) (y - 2) 1 1;
  fill_rect (x - 2) y 1 1;
  fill_rect (x - 2) (y - 3) 1 1

let draw_orange (x, y) =
  let y = size_y () - y - 10 in
  let x = x + 8 in
  draw_fruit_body (x, y) (rgb 255 165 0) 4;
  set_color green;
  fill_rect x (y + 5) 3 1

let draw_apple (x, y) =
  let y = size_y () - y - 10 in
  let x = x + 8 in
  draw_fruit_body (x, y) red 4;
  set_color green;
  fill_rect (x - 3) (y + 4) 3 1

let draw_melon (x, y) =
  let y = size_y () - y - 10 in
  let x = x + 8 in
  draw_fruit_body (x, y) (rgb 0 255 0) 4;
  set_color (rgb 0 200 0);
  draw_poly_line [| (x - 3, y + 5); (x + 2, y + 5) |];
  set_color (rgb 34 139 34);
  fill_rect (x - 2) (y - 2) 1 1;
  fill_rect (x + 2) (y - 1) 1 1;
  fill_rect x (y + 2) 1 1

let draw_path coords = draw_solid_tile coords black

let draw_jail_exit (x, y) =
  draw_solid_tile (x, y) black;
  Graphics.set_color Graphics.red;
  let ly = y + (tile_size / 2) in
  let adjy = size_y () - ly in
  draw_poly_line [| (x, adjy); (x + tile_size, adjy) |]

let draw_wall coords = draw_solid_tile coords wall_color

let draw_dot (x, y) is_big =
  let point_radius = if is_big then 4 else point_radius in
  draw_solid_tile (x, y) Graphics.black;
  Graphics.set_color point_filling_color;
  let adjusted_y = Graphics.size_y () - y - tile_size + (tile_size / 2) in
  Graphics.draw_circle (x + (tile_size / 2)) adjusted_y point_radius;
  Graphics.fill_circle (x + (tile_size / 2)) adjusted_y point_radius
