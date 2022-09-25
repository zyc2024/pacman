open Game
open Graphics

exception Victory
exception Defeat

let game = ref (GameState.init_game 28 31 0 3)
let level = ref 0

(* this is the all-time high score recorded on the player's local computer, or 0
   if one doesn't exist yet. *)
let highscore () =
  try
    let json =
      Yojson.Basic.from_file "highscore.json" |> Yojson.Basic.Util.to_assoc
    in
    json |> List.assoc "high score" |> Yojson.Basic.Util.to_int
  with _e -> 0

(** [update_highscore] writes to a local json file to keep track of the highest
    score recorded on the player's computer. *)
let update_highscore (game : GameState.t) =
  let score = GameState.get_score game in
  if score > highscore () then
    Yojson.to_file "highscore.json" (`Assoc [ ("high score", `Int score) ])

(** [close_gracefully] ensures the game closes cleanly. *)
let close_gracefully (game : GameState.t) =
  update_highscore game;
  Graphics.close_graph ()

(** [spinwait time] makes the game do nothing but wait for [time] seconds. If
    the time argument is omitted, it waits indefinitely. *)
let spinwait ?(time = ~-.1.) () =
  let t = Sys.time () in
  while if time > 0. then Sys.time () -. t < time else true do
    ()
  done

(** [draw_restart_note ()] draws the restart instructions*)
let draw_restart_note () =
  Graphics.moveto ((Graphics.size_x () / 2) - 75) ((Graphics.size_y () / 2) - 50);
  Graphics.draw_string "Press \"r\" to play again."

(** [prepare_graphics ()] clears the current graph and sets the properties for
    drawing strings*)
let prepare_graphics () =
  clear_graph ();
  set_color black;
  set_text_size 20;
  moveto ((size_x () / 2) - 50) (size_y () / 2)

(** [draw_final_score ()] displays the final score of this game attempt*)
let draw_final_score game =
  moveto ((size_x () / 2) - 50) ((size_y () / 2) - 25);
  draw_string ("FINAL SCORE: " ^ string_of_int (GameState.get_score game))

(** [next_state game] computes the next frame state after moving all ghosts,
    pacman, taking new user input, fruit check, etc*)
let next_state game =
  game |> GameState.move |> GameState.eat |> GameState.collision
  |> GameState.update_ghost_mode !level
  |> GameState.update_ghost_jail_state |> GameState.update_fruit
  |> GameState.user_input

(** [player_alive_check game] checks whether the player is alive in [game] and
    either modify the game for the next frame or reinitialize the game when the
    player is found dead. If the player is out of lives, the player is defeated.*)
let player_alive_check game =
  if GameState.is_alive !game then (
    game := next_state !game;
    spinwait ~time:(1. /. 90.) ())
  else (
    spinwait ~time:1.5 ();
    if GameState.num_lives !game = 0 then raise Defeat
    else game := GameState.reinit !game)

(** [victory game] displays the victory screen once a player wins. *)
let rec victory (game : GameState.t) =
  try
    auto_synchronize true;
    spinwait ~time:2.0 ();
    prepare_graphics ();
    if !level < 3 then begin
      draw_string "Get ready for the next level.";
      level := !level + 1;
      spinwait ~time:5.0 ();
      main (GameState.get_score game) (GameState.num_lives game)
    end
    else begin
      draw_string "YOU WON!";
      draw_final_score game;
      restart game
    end
  with _e -> close_gracefully game

(** [defeat game] displays the gameover screen once a player loses. *)
and defeat (game : GameState.t) =
  try
    auto_synchronize true;
    prepare_graphics ();
    draw_string "YOU DIED.";
    draw_final_score game;
    restart game
  with _e -> close_gracefully game

(** [restart game] prompts the user to press r key to reinitialize game*)
and restart game =
  try
    draw_restart_note ();
    while true do
      if GameState.restart () then (
        update_highscore game;
        level := 0;
        main 0 3)
    done
  with _e -> close_gracefully game

and main score lives =
  try
    game := GameState.init_game 28 31 score lives;
    GameState.init_display !game;
    GameState.draw_screen !game (highscore ());
    spinwait ~time:2.0 ();
    while true do
      Graphics.auto_synchronize false;
      if GameState.game_won !game then raise Victory
      else if GameState.paused !game then game := GameState.user_input !game
      else player_alive_check game;
      GameState.draw_screen !game (highscore ());
      Graphics.auto_synchronize true
    done
  with
  | Victory -> victory !game
  | Defeat -> defeat !game
  | _ -> close_gracefully !game

let () = main 0 3
