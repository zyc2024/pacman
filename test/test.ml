(** Testing plan: In general, we will test every component module and
    attempt to achieve 100 percent Bisect coverage. In developing test
    cases, we begin with black box testing techniques and aim for
    correct outputs based on specification. To achieve high bisect
    coverage, we will perform white box testing on individual functions
    to capture all possible branches. We performed OUnit testing on the
    following modules : [Maze], [MazeBuilder], [Pacman], [Ghosts],
    [GhostBehavior], and [Direction]. We omitted the automated testing
    of any graphics (such as those in module [TileDraw]) and user
    interaction dependent/related functions such as in [GameState] and
    [Main]. For those that we omitted, we consistently interacted with
    the game for accurate outputs. We believe our test suite
    demonstrates the correctness of our system because we aimed to
    capture the majority (if not all) of the potential behaviorial
    outcomes of individual components of our game through automated
    coverage testing, and we manually tested the components as a whole
    (the game user interface) to ensure they function together rather
    than solely individually*)

open OUnit2
open Components
open Characters
open Util.Direction

(********************************************************************
   Here are some helper functions. 
 ********************************************************************)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_h2 :: _t as t') ->
          if n = 100 then acc ^ "..."
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"
  [@@coverage off]

(** [pp_tuple tp] pretty-prints a tuple*)
let pp_tuple pp_elt tp =
  "(" ^ pp_elt (fst tp) ^ "," ^ pp_elt (snd tp) ^ ")"

(** [dir_to_str dir] converts [dir] to string*)
let dir_to_str = function
  | North -> "North"
  | South -> "South"
  | East -> "East"
  | West -> "West"

(** [mode_to_str mode] converts ghost [mode] to string*)
let mode_to_str mode =
  let open Ghosts in
  match mode with
  | Scatter -> "Scatter"
  | Chase -> "Chase"
  | Frightened -> "Frightened"
  | Dead -> "Dead"
  [@@coverage off]

(** [pp_dir dir] pretty-prints a direction*)
let pp_dir dir = pp_string (dir_to_str dir)

(** [pp_coord (x,y)] pretty-prints [(x,y)]*)
let pp_coord = pp_tuple string_of_int

(** [loop i acc f] is [f_i (... f_1 (acc))]*)
let rec loop i acc f = if i = 0 then acc else loop (i - 1) (f acc) f

(** [lo -- hi] is the list containing the integers from [lo] to [hi],
    inclusive. For example, [0 -- 3] is [0; 1; 2; 3], and [4 -- 3] is
    [\[\]]. The implementation is tail recursive. (Credit to A0)*)
let ( -- ) lo hi =
  let rec loop lo hi acc =
    if lo > hi then acc else loop (lo + 1) hi (lo :: acc)
  in
  List.rev (loop lo hi [])

let test name expected func input print =
  name >:: fun _ -> assert_equal expected (func input) ~printer:print

let fruit_point_test name map i o =
  test name o (Maze.fruit_points map) i string_of_int

(********************************************************************
   End helper functions.
 ********************************************************************)

module MazeTest = struct
  open Maze

  (* note: map is mutable. All tests needs to agree on the same version.*)
  let map = grid_init 10 7

  (* alias for make_struct for 10x7 map*)
  let make s pos = make_struct map s pos

  (* alias for has_struct for 10x7 map*)
  let has s pos = has_struct map s pos

  let _build_structs =
    List.iter (fun i -> make Wall (i * 16, 16)) (1 -- 8);
    let structs =
      [ Fruit Strawberry; Dot; PowerDot; Wall; Path; JailExit; Path ]
    in
    List.iter (fun s -> make s (0, 0)) structs;
    List.iter (fun i -> make JailExit (i * 16, 16)) (4 -- 5);
    make Dot (0, 96);
    make PowerDot (144, 0);
    make (Fruit Cherry) (144, 96)

  let fruit_map = grid_init 2 3

  let _build_fruits =
    make_struct fruit_map (Fruit Strawberry) (0, 0);
    make_struct fruit_map (Fruit Apple) (16, 0);
    make_struct fruit_map (Fruit Orange) (0, 16);
    make_struct fruit_map (Fruit Melon) (16, 16);
    make_struct fruit_map (Fruit Cherry) (0, 32);
    make_struct fruit_map Path (16, 32)

  (* alias for has_some_fruit for fruit_map*)
  let has_fruit = has_some_fruit fruit_map

  let structure_test =
    [
      test "width of map 28x31" 10 width map string_of_int;
      test "height of map 28x31" 7 height map string_of_int;
      test "normalize pos coord" (0, 0) (normalize map) (160, 0)
        pp_coord;
      test "normalize neg coord"
        (9 * 16, 0)
        (normalize map) (-16, 0) pp_coord;
      test "make wall -> has wall" true (has Wall) (16, 16)
        string_of_bool;
      test "make [s1 -> ... -> sn], has sn" true (has Path) (0, 0)
        string_of_bool;
      test "has jail-exit" true (has JailExit) (64, 16) string_of_bool;
      test "has powerup" true (has PowerDot) (144, 0) string_of_bool;
      test "dot is not powerup" false (has PowerDot) (0, 96)
        string_of_bool;
      test "has cherry" true (has (Fruit Cherry)) (144, 96)
        string_of_bool;
      test "fruit tile is not empty" false (has Path) (144, 96)
        string_of_bool;
      test "has some fruit (strawberry)" true has_fruit (8, 8)
        string_of_bool;
      test "has some fruit (melon)" true has_fruit (16, 16)
        string_of_bool;
      test "has some fruit (orange)" true has_fruit (0, 16)
        string_of_bool;
      test "has some fruit (apple)" true has_fruit (16, 0)
        string_of_bool;
      test "has some fruit (cherry)" true has_fruit (0, 32)
        string_of_bool;
      test "has no fruit" false has_fruit (16, 32) string_of_bool;
      test "small map has no coins" 0
        (count_struct fruit_map)
        Dot string_of_int;
      test "map has 1 powerup" 1 (count_struct map) PowerDot
        string_of_int;
      fruit_point_test "cherry pts" map (144, 96) 100;
      fruit_point_test "strawberry pts" fruit_map (0, 0) 300;
      fruit_point_test "orange pts" fruit_map (0, 16) 500;
      fruit_point_test "apple pts" fruit_map (16, 0) 700;
      fruit_point_test "melon pts" fruit_map (16, 16) 1000;
      ( "not a fruit" >:: fun _ ->
        assert_raises (Failure "precondition violated") (fun () ->
            fruit_points map (0, 0)) );
    ]

  (** alias for neighbor_by_dir for this map*)
  let neighbor pos dir = neighbor_by_dir map pos dir

  (** alias for moved_pos for this map*)
  let moved pos d dir = moved_pos map pos d dir

  (** alias for distance for this map*)
  let dist c1 c2 = distance map c1 c2

  (** alias for travel_dirs for this map*)
  let dirs c1 c2 = travel_dirs map c1 c2

  let mechanics_test =
    [
      test "tile E of (0,0)" (16, 0) (neighbor (0, 0)) East pp_coord;
      test "tile W of (0,0)" (144, 0) (neighbor (0, 0)) West pp_coord;
      test "tile N of (0,0)" (0, 96) (neighbor (0, 0)) North pp_coord;
      test "tile S of (96,0)" (0, 0) (neighbor (0, 96)) South pp_coord;
      test "all neighbors of (0,0)"
        [ (0, 96); (144, 0); (0, 16); (16, 0) ]
        (Maze.get_neighbors map)
        (0, 0) (pp_list pp_coord);
      test "2 units E of (0,0)" (2, 0) (moved (0, 0) 2) East pp_coord;
      test "2 units W of (0,0)" (158, 0) (moved (0, 0) 2) West pp_coord;
      test "2 units N of (0,0)" (0, 110) (moved (0, 0) 2) North pp_coord;
      test "2 units S of (0,0)" (0, 2) (moved (0, 0) 2) South pp_coord;
      test "tile dist between (0,0) & (144,0)" 144.0
        (dist (0, 0))
        (144, 0) string_of_float;
      test "go up through jail exit" (64, 30)
        (entity_move map (64, 32) North)
        2 pp_coord;
      test "cannot go down jail exit" (64, 0)
        (entity_move map (64, 0) South)
        2 pp_coord;
      test "dirs from (0,0) to (16,16)" [ South; East ]
        (dirs (0, 0))
        (16, 16) (pp_list pp_dir);
      test "dirs from (0,0) to (0,16)" [ South ]
        (dirs (0, 0))
        (0, 16) (pp_list pp_dir);
      test "dirs from (32,0) to (16,16)" [ West; South ]
        (dirs (32, 0))
        (16, 16) (pp_list pp_dir);
    ]

  let tests = structure_test @ mechanics_test
end

(* Use sample small map for testing (non-valid-map dependent) pacman and
   ghosts features. This is a 10x10 dot maze with two horizontal
   partial-jail exits and walls at (16,0), (0,32) and (48,0) *)
let maze = Maze.grid_init 10 10

let _build =
  List.iter
    (fun pos -> Maze.make_struct maze Wall pos)
    [ (16, 0); (48, 0); (0, 32) ];
  List.iter
    (fun pos -> Maze.make_struct maze JailExit pos)
    [ (48, 32); (64, 32) ]

module PacmanTest = struct
  open Pacman

  let update_dir = change_dir maze

  let pac_west = create (0, 0) West

  (* pacman facing west, move east whenever possible*)
  let pac_we = update_dir pac_west East

  (* pacman facing north, moves north only*)
  let pac_north = update_dir pac_west North

  (* pacman facing south, moves east whenever possible*)
  let pac_se = update_dir (update_dir pac_north South) East

  (* pacman wants west or east, but neither is valid*)
  let pac_still = update_dir (create (32, 0) East) West

  let tests =
    let pmove = move maze in
    [
      test "pacman at position (0, 0)" (0, 0) get_coords pac_west
        pp_coord;
      test "pacman facing west" West facing_dir pac_west pp_dir;
      test "changes direction to North" North facing_dir pac_north
        pp_dir;
      test "move west (wrap) to (158, 0)" (158, 0) get_coords
        (pmove pac_west) pp_coord;
      test "cannot move east, moves west" (158, 0) get_coords
        (pmove pac_we) pp_coord;
      test "cannot move, updates to face east" East facing_dir
        (update_dir pac_still East)
        pp_dir;
      test "move North (wrap) to (0, 158)" (0, 158) get_coords
        (pmove pac_north) pp_coord;
      (let n_then_s = update_dir (pmove pac_north) South |> pmove in
       test "move North (0, 158) and move south back to (0,0)" (0, 0)
         get_coords n_then_s pp_coord);
      test "South until wall then move East (alt)" East facing_dir
        (loop 9 pac_se (move maze))
        pp_dir;
    ]
end

module GhostTests = struct
  open Ghosts

  let blinky = Ghosts.create Blinky (0, 0) North

  let pinky = Ghosts.create Pinky (16, 16) East

  let inky = Ghosts.create Inky (32, 0) West

  let clyde = Ghosts.create Clyde (48, 18) South

  let pacman = Pacman.create (0, 0) South

  let gmove = move maze

  let transition_x =
    let blinky = gmove blinky West 1 in
    gmove blinky West 2

  let transition_y =
    let blinky = gmove blinky North 1 in
    gmove blinky North 2

  let dead_clyde =
    let dead_ghost = Ghosts.set_mode Dead clyde in
    gmove dead_ghost South 4

  let tests =
    [
      test "Blinky at (0,0)" (0, 0) get_coords blinky pp_coord;
      test "Pinky at (16,16)" (16, 16) get_coords pinky pp_coord;
      test "Inky at (32,0)" (32, 0) get_coords inky pp_coord;
      test "Clyde at (48,18)" (48, 18) get_coords clyde pp_coord;
      test "Blinky face North" North get_dir blinky pp_dir;
      test "Pinky face East" East get_dir pinky pp_dir;
      test "Inky face West" West get_dir inky pp_dir;
      test "Clyde face South" South get_dir clyde pp_dir;
      test "Blinky fails East, moves North to (0, 158)" (0, 158)
        get_coords (gmove blinky East 2) pp_coord;
      test "Pinky moves East by 2" (18, 16) get_coords
        (gmove pinky East 2) pp_coord;
      test "frighten mode to chase (x axis)" (156, 0) get_coords
        transition_x pp_coord;
      test "frighten mode to chase (y axis)" (0, 156) get_coords
        transition_y pp_coord;
      test "dead ghost enters jail-gate" (48, 24) get_coords dead_clyde
        pp_coord;
      test "Ghost update mode" Frightened get_mode
        (set_mode Frightened blinky) (fun m ->
          pp_string (mode_to_str m));
      test "ghost pacman collide" true
        (collision blinky pacman)
        maze string_of_bool;
    ]
end

module DirectionTests = struct
  let tests =
    [
      test "opposite of South" North opposite South pp_dir;
      test "opposite of North" South opposite North pp_dir;
      test "opposite of West" East opposite West pp_dir;
      test "oppoosite of East" West opposite East pp_dir;
      test "North is opposite of East?" false
        (fun d -> opposite East = d)
        North string_of_bool;
      test "complement of [NS]" [ West; East ] complement
        [ North; South ] (pp_list pp_dir);
    ]
end

module MazeBuilderTests = struct
  open MazeBuilder
  open Ghosts

  let map = build_level 28 31 "map.json"

  let tests =
    [
      ( "nonexisting file for building map" >:: fun _ ->
        assert_raises (Failure "parsing failed") (fun () ->
            build_level 10 10 "jack") );
      ( "incorrect data formatting" >:: fun _ ->
        assert_raises (Failure "incorrect formatting") (fun () ->
            build_level 10 10 "test_samples/sample1.json") );
      ( "incorrect json missing data attr" >:: fun _ ->
        assert_raises (Failure "required data not found") (fun () ->
            build_level 10 10 "test_samples/sample2.json") );
      ( "invalid game-map -> no exit" >:: fun _ ->
        assert_raises (Failure "invalid map") (fun () ->
            Maze.get_jail_coords
              (build_level 10 10 "test_samples/sample3.json")) );
      test "valid map -> valid exit" (208, 192) Maze.get_jail_coords map
        pp_coord;
      test "initial ghost is in jail" true is_in_jail
        (update_jail_status (create Blinky (208, 192) North) map)
        string_of_bool;
      test "ghost update leaving jail" false is_in_jail
        (update_jail_status (create Blinky (208, 176) North) map)
        string_of_bool;
      test "ghost coming into jail" true is_in_jail
        (update_jail_status (create Blinky (208, 192) South) map)
        string_of_bool;
      test "pacman map has 4 powerups" 4 (Maze.count_struct map)
        PowerDot string_of_int;
      test "pacman map has 246 coins" 246 (Maze.count_struct map) Dot
        string_of_int;
    ]
end

module BehaviorTests = struct
  open Ghosts
  open GhostBehavior

  let m = MazeBuilder.build_level 28 31 "map.json"

  let gmove name ?(in_jail = false) mode bpos bdir tpos tdir =
    let g =
      create name bpos bdir |> set_mode mode |> set_in_jail in_jail
    in
    let p = Pacman.create tpos tdir in
    match name with
    | Blinky -> BlinkyBehavior.move g m p
    | Pinky -> PinkyBehavior.move g m p
    | Clyde -> ClydeBehavior.move g m p
    | Inky ->
        let b = create Blinky (208, 368) East in
        InkyBehavior.move g b m p

  let tests =
    [
      ( "ghost (scatter) in jail, head towards exit",
        gmove Blinky ~in_jail:true Scatter (208, 192) West (16, 16)
          South,
        (208, 190) );
      ( "ghost (chase) in jail, head towards exit",
        gmove Blinky ~in_jail:true Chase (208, 192) East (16, 16) South,
        (208, 190) );
      ( "Blinky(S) chases pacman(S)",
        gmove Blinky Chase (16, 16) South (16, 32) South,
        (16, 18) );
      ( "Blinky(N) fails to chase pacman(S), moves E",
        gmove Blinky Chase (16, 16) North (16, 32) South,
        (18, 16) );
      ( "Blinky scattering top right",
        gmove Blinky Scatter (416, 16) North (16, 16) South,
        (414, 16) );
      ( "Pinky(W) tries ambush Pacman(E)",
        gmove Pinky Chase (240, 368) West (192, 320) East,
        (240, 366) );
      ( "Pinky(E) forced to distance from Pacman(E)",
        gmove Pinky Chase (32, 16) East (16, 16) East,
        (34, 16) );
      ( "scared Pinky(E) pick best of bad directions",
        gmove Pinky Frightened (240, 80) East (416, 16) West,
        (240, 79) );
      ( "Clyde(E) distance from pacman (too close)",
        gmove Clyde Chase (240, 80) East (240, 64) South,
        (242, 80) );
      ( "Clyde(E) moves E towards pacman (far)",
        gmove Clyde Chase (16, 16) East (240, 80) East,
        (18, 16) );
      ( "scattering Clyde(S) at closest point (left bottom)",
        gmove Clyde Scatter (16, 464) South (16, 16) West,
        (18, 464) );
      ( "dead ghost right above exit, moves S",
        gmove Clyde Dead (208, 176) West (16, 16) East,
        (208, 180) );
      ( "dead ghost comes into exit, moves regular speed",
        gmove Clyde Dead (208, 192) South (16, 16) East,
        (208, 194) );
      ( "inky + blinky sandwich pacman (approach)",
        gmove Inky Chase (288, 368) North (240, 368) West,
        (286, 368) );
      ( "inky + blinky sandwich pacman (distancing)",
        gmove Inky Chase (256, 368) North (240, 368) West,
        (258, 368) );
    ]
end

let behaviorTests =
  List.map
    (fun (n, i, o) -> test n o Ghosts.get_coords i pp_coord)
    BehaviorTests.tests

let suite =
  "test suite for pacman game"
  >::: List.flatten
         [
           MazeTest.tests;
           PacmanTest.tests;
           GhostTests.tests;
           DirectionTests.tests;
           MazeBuilderTests.tests;
           behaviorTests;
         ]

let _ = run_test_tt_main suite
