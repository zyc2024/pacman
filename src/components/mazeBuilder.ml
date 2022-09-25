exception ParseError of string

(** [perimeter_wall w h] is a list of indices in which a [Wall] will
    occupy on a maze of width [w] and height [h].*)
let perimeter_wall w h =
  let rec loop i max f acc =
    if i >= max then acc else loop (i + 1) max f (f i :: acc)
  in
  let left = loop 0 h (fun b -> (0, b)) [] in
  let top = loop 1 (w - 1) (fun a -> (a, 0)) left in
  let right = loop 0 h (fun b -> (w - 1, b)) top in
  loop 1 (w - 1) (fun a -> (a, h - 1)) right

(** [data key json] is the string data corresponding to the [key] in
    data [json].*)
let data key json =
  try
    let lst = json |> List.assoc key |> Yojson.Basic.Util.to_list in
    List.map (fun s -> Yojson.Basic.Util.to_string s) lst
  with _ -> raise (ParseError "required data not found")

(** [coord_set key filename] converts data associated with key into
    coordinate pairs*)
let coord_set key file_name =
  let rec loop lst acc =
    let parse s lst =
      match String.split_on_char ',' s with
      | [ i; j; w; h ] ->
          let i = i |> int_of_string in
          let j = j |> int_of_string in
          let w = w |> int_of_string in
          let h = h |> int_of_string in
          for a = 0 to w - 1 do
            for b = 0 to h - 1 do
              acc := (i + a, j + b) :: !acc
            done
          done;
          loop lst acc
      | _ -> raise (ParseError "incorrect formatting")
    in
    match lst with [] -> !acc | h :: t -> parse h t
  in
  loop (data key file_name) (ref [])

let build_level w h file_name =
  try
    let json =
      Yojson.Basic.from_file file_name |> Yojson.Basic.Util.to_assoc
    in
    let maze = ref (Maze.grid_init w h) in
    let rec loop lst s =
      match lst with
      | [] -> ()
      | (x, y) :: t ->
          Maze.make_struct !maze s (16 * x, 16 * y);
          loop t s
    in
    loop (perimeter_wall (Maze.width !maze) (Maze.height !maze)) Wall;
    loop (coord_set "walls" json) Wall;
    loop (coord_set "empty" json) Path;
    loop (coord_set "powerups" json) Maze.PowerDot;
    loop (coord_set "pen-exit" json) JailExit;
    !maze
  with
  | ParseError s -> failwith s
  | _ -> failwith "parsing failed"
