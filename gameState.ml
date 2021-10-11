open Direction
open GhostBehavior

type t = {
  pacman : Pacman.t;
  (* List order: Blinky, Pinky, Inky, Clyde *)
  ghosts : Ghosts.t list;
  maze : Maze.t;
  tile_size : int;
  coin_count : int;
  score : int;
  alive : bool;
  lives : int;
  is_paused : bool;
}

(** [fruit_spawner] contains all the information regarding the fruit
    spawning process (fruits remaining, duration, delay)*)
type fruit_spawner = {
  (* The list of fruits that haven't been used yet in this level. *)
  lst : (Maze.fruit * (int * int)) list ref;
  (* The amount of time a fruit is on the screen (constant) *)
  active_time : float;
  (* The amount of time a fruit is off the screen.*)
  down_time : float ref;
  (* The start time of the latest fruit event. *)
  timer : float ref;
  (* The coordinate of a fruit in Maze. None if no fruit*)
  coords : (int * int) option ref;
}

(** [ghost_cycle] contains all the information regarding the ghost-mode
    swapping process (scatter to chase and vice versa, durations)*)
type ghost_cycle = {
  (* the amount of scatter cycles*)
  scatter_count : int ref;
  (* the time spent in chase mode per mode switch*)
  chase_time : float;
  (* the time spent in frightened mode per activation of power-up for
     each level. Decreasing in total time across levels*)
  frightened_time : float list;
  (* the start time of the latest ghost event*)
  timer : float ref;
}

let get_score (game : t) = game.score

let game_won (game : t) = game.coin_count = 0

let paused (game : t) = game.is_paused

let is_alive (game : t) = game.alive

let num_lives (game : t) = game.lives

let init_display (game : t) =
  Graphics.(
    open_graph " 800*600";
    set_window_title "Pac-man";
    resize_window 450 590)

(** [print_score] is a helper used to print the player's current score
    as well as the all-time high score recorded on the player's
    computer. *)
let print_score (game : t) (highscore : int) =
  Graphics.(
    Graphics.set_color Graphics.white;
    set_text_size 20;
    moveto 200 (Graphics.size_y () - 525);
    draw_string ("Score: " ^ string_of_int game.score);
    moveto 185 (Graphics.size_y () - 550);
    draw_string ("High Score: " ^ string_of_int highscore);
    moveto 150 (Graphics.size_y () - 575);
    draw_string
      ("Press the spacebar to "
      ^ if game.is_paused then "unpause." else "pause."))

(** [draw_lives pos lives] draws the number of lives the player has
    starting at [pos] and stops when [lives] is 0. *)
let rec draw_lives game pos = function
  | x when x > 0 ->
      let p = Pacman.create pos East in
      Pacman.draw p;
      draw_lives game (fst pos - (2 * game.tile_size), snd pos) (x - 1)
  | _ -> ()

let draw_screen game highscore =
  Graphics.(
    clear_graph ();
    set_color black;
    fill_rect 0 0 (size_x ()) (size_y ()));
  Maze.draw_maze game.maze;
  Pacman.draw game.pacman;
  List.iter Ghosts.draw game.ghosts;
  draw_lives game (395, Graphics.size_y () - 78) game.lives;
  print_score game highscore

(** Contains the locations and types of all five fruits *)
let inital_fruit_list =
  let open Maze in
  let pos = (13 * 16, 17 * 16) in
  List.map
    (fun f -> (f, pos))
    [ Cherry; Strawberry; Orange; Apple; Melon ]

let spawner =
  {
    lst = ref inital_fruit_list;
    active_time = 10.0;
    down_time = ref 15.;
    timer = ref (Sys.time ());
    coords = ref None;
  }

(** [reset_fruit_timer ()] resets the current fruit timer. *)
let reset_fruit_timer () = spawner.timer := Sys.time ()

(** [update_down_time ()] updates the fruit spawn delay time*)
let update_down_time () =
  spawner.down_time :=
    float_of_int (Random.int 10 + 15)
    +. (float_of_int (Random.int 100) /. 100.)

(** [set_fruit maze ()] either does nothing when pacman has consumed all
    the fruits for the level or deploys the next fruit*)
let set_fruit maze () =
  match !(spawner.lst) with
  | [] -> ()
  | h :: t ->
      spawner.lst := t;
      spawner.coords := Some (snd h);
      reset_fruit_timer ();
      Maze.(make_struct maze (Fruit (fst h)) (snd h))

let update_fruit game =
  let _ =
    match !(spawner.coords) with
    | Some (x, y) ->
        if Sys.time () -. !(spawner.timer) >= spawner.active_time then (
          spawner.coords := None;
          update_down_time ();
          reset_fruit_timer ();
          Maze.make_struct game.maze Path (x, y))
    | None ->
        if Sys.time () -. !(spawner.timer) >= !(spawner.down_time) then
          set_fruit game.maze ()
  in
  game

(** [toggle mode gs] updates all ghost modes to [mode]. Note: Dead
    ghosts stay dead. *)
let toggle mode gs =
  Ghosts.(
    List.map
      (fun g ->
        match get_mode g with Dead -> g | _ -> set_mode mode g)
      gs)

let gcycle =
  {
    scatter_count = ref 4;
    chase_time = 15.;
    frightened_time = [ 8.; 6.5; 5.; 2.5 ];
    timer = ref (Sys.time ());
  }

(** List of scatter times to use as the level progresses. *)
let scatter_time = [ 9.; 6.; 3.; 0. ]

(** [reset_ghost_timer] resets the current ghost mode timer. *)
let reset_ghost_timer () = gcycle.timer := Sys.time ()

(** [reset_ghost_modes] resets the timer and scatter count. *)
let reset_ghost_modes () =
  reset_ghost_timer ();
  gcycle.scatter_count := 4

(** [mode_finder ghosts] is the current mode of all [ghosts]. This is
    [Frightened] if at least one ghost is [Frightened]. Otherwise, it is
    the normal mode of the ghosts. *)
let mode_finder ghosts =
  let open Ghosts in
  let modes = List.map get_mode ghosts in
  match List.find_opt (fun m -> m = Frightened) modes with
  | Some _ -> Frightened
  | None -> List.hd modes

(** [update_gmode_aux glist mode] sets [mode] as the mode of every ghost
    in [glist]*)
let update_gmode_aux glist mode =
  reset_ghost_timer ();
  toggle mode glist

let update_ghost_mode level game =
  let mode = mode_finder game.ghosts in
  let glst = ref game.ghosts in
  let elapsed_time = Sys.time () -. !(gcycle.timer) in
  let scatter_enabled = !(gcycle.scatter_count) > 0 in
  (match mode with
  | Scatter ->
      if elapsed_time >= List.nth scatter_time level then
        glst := update_gmode_aux !glst Chase
  | Chase ->
      if scatter_enabled && elapsed_time >= gcycle.chase_time then (
        gcycle.scatter_count := !(gcycle.scatter_count) - 1;
        glst := update_gmode_aux !glst Scatter)
  | Frightened ->
      if elapsed_time >= List.nth gcycle.frightened_time level then
        glst :=
          update_gmode_aux !glst
            (if scatter_enabled then Scatter else Chase)
  | Dead -> ());
  { game with ghosts = !glst }

let update_ghost_jail_state game =
  let ghosts = game.ghosts in
  let maze = game.maze in
  {
    game with
    ghosts = List.map (fun g -> Ghosts.update_jail_status g maze) ghosts;
  }

(* credit to
   https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)

(** [shuffle l] shuffles the list [l]. *)
let shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort compare nd in
  List.map snd sond

(** the middle x position for which Blinky, Pinky, and Pacman spawns*)
let ox = int_of_float (13.5 *. 16.)

(** the list of ghosts with their initial positions and states*)
let init_ghosts =
  [
    Ghosts.create Blinky (ox, 11 * 16) North;
    Ghosts.create Pinky (ox, 14 * 16) West;
    Ghosts.create Inky (ox - 24, 14 * 16) East;
    Ghosts.create Clyde (ox + 24, 14 * 16) West;
  ]

let init_game w h score lives =
  spawner.lst := shuffle inital_fruit_list;
  reset_fruit_timer ();
  reset_ghost_modes ();
  let maze = MazeBuilder.build_level w h "map.json" in
  {
    pacman = Pacman.create (ox, 23 * 16) West;
    ghosts = init_ghosts;
    tile_size = 16;
    coin_count =
      Maze.count_struct maze Dot + Maze.count_struct maze PowerDot;
    alive = true;
    is_paused = false;
    maze;
    score;
    lives;
  }

let reinit (game : t) =
  reset_fruit_timer ();
  reset_ghost_modes ();
  {
    game with
    pacman = Pacman.create (ox, 23 * 16) West;
    ghosts = init_ghosts;
    alive = true;
  }

let restart () =
  let read (status : Graphics.status) =
    if status.keypressed then
      match
        (Graphics.wait_next_event [ Key_pressed ]).key
        |> Char.lowercase_ascii
      with
      | 'r' -> true
      | _ -> false
    else false
  in
  Graphics.wait_next_event [ Poll ] |> read

(** [new_state game dir] updates the pacman in [game] with [dir] iff the
    game is not paused.*)
let new_state game dir =
  if not game.is_paused then
    { game with pacman = Pacman.change_dir game.maze game.pacman dir }
  else game

let user_input game =
  let toggle_pause game =
    { game with is_paused = not game.is_paused }
  in
  let read (status : Graphics.status) =
    if status.keypressed then
      match
        (Graphics.wait_next_event [ Key_pressed ]).key
        |> Char.lowercase_ascii
      with
      | ' ' -> toggle_pause game
      | 'w' -> new_state game North
      | 'a' -> new_state game West
      | 's' -> new_state game South
      | 'd' -> new_state game East
      | _ -> game
    else game
  in
  Graphics.wait_next_event [ Poll ] |> read

let move (game : t) =
  let maze = game.maze in
  let pac = game.pacman in
  let ghosts = game.ghosts in
  {
    game with
    pacman = Pacman.move maze pac;
    ghosts =
      [
        BlinkyBehavior.move (List.nth ghosts 0) maze pac;
        PinkyBehavior.move (List.nth ghosts 1) maze pac;
        InkyBehavior.move (List.nth ghosts 2) (List.nth ghosts 0) maze
          pac;
        ClydeBehavior.move (List.nth ghosts 3) maze pac;
      ];
  }

(** [eat_pellet_aux game pos item] is [game] with the tile at position
    [pos] cleared, with score updated, and ghosts frightened if [item]
    is a power up. *)
let eat_pellet_aux (game : t) pos item =
  let dot_eaten_state =
    Maze.make_struct game.maze Path pos;
    {
      game with
      coin_count = game.coin_count - 1;
      score = game.score + 10;
    }
  in
  match item with
  | Maze.Dot -> dot_eaten_state
  | Maze.PowerDot ->
      {
        dot_eaten_state with
        ghosts = toggle Frightened dot_eaten_state.ghosts;
      }
  | _ -> failwith "impossible"

let eat (game : t) =
  let pacman_pos = Pacman.get_coords game.pacman in
  let maze = game.maze in
  let has_dot = Maze.has_struct maze Dot pacman_pos in
  let has_power = Maze.has_struct maze PowerDot pacman_pos in
  match (has_dot, has_power) with
  | true, false -> eat_pellet_aux game pacman_pos Dot
  | false, true ->
      reset_ghost_timer ();
      eat_pellet_aux game pacman_pos PowerDot
  | _ ->
      if Maze.has_some_fruit maze pacman_pos then (
        let points = Maze.fruit_points maze pacman_pos in
        Maze.make_struct maze Path pacman_pos;
        { game with score = game.score + points })
      else game

(** [is_frightened ghost] is true iff [ghost]'s mode is
    [Ghosts.Frightened]*)
let is_frightened ghost = Ghosts.get_mode ghost = Ghosts.Frightened

(** [is_dead ghost] is true iff [ghost] is [Dead]. *)
let is_dead ghost = Ghosts.get_mode ghost = Ghosts.Dead

(* delay effect*)
let short_pause t s =
  while Sys.time () -. t < s do
    ()
  done

(* [collision_effects game alive score lives g] reduces the number of
   lives iff an [alive] pacman in [game] collides with [g]. If [g] is
   frightened and pacman [alive] then [score] gets incremented.
   Otherwise, there will be no side effects.*)
let collision_effects game alive score lives (g : Ghosts.t) =
  if
    !alive
    && (not (is_dead g))
    && Ghosts.collision g game.pacman game.maze
  then
    if is_frightened g then begin
      score := !score + 400;
      short_pause (Sys.time ()) 0.25;
      Ghosts.set_mode Dead g
    end
    else (
      lives := !lives - 1;
      alive := false;
      g)
  else g

let collision (game : t) =
  let score = ref game.score in
  let alive = ref game.alive in
  let lives = ref game.lives in
  let updated_ghosts =
    List.map
      (fun g -> collision_effects game alive score lives g)
      game.ghosts
  in
  {
    game with
    ghosts = updated_ghosts;
    alive = !alive;
    score = !score;
    lives = !lives;
  }
