open Graphics
open Board
open Game
open State
open Command

(** [run_game game st] is the main game loop for the Tetris game. It checks
    for keyboard input and executes the appropriate behavior.*)
let rec run_game game st  = 
  try let x = keyboard game st in x |> update game |> run_game game with 
  | NoKeyPress -> run_game game (update game st)
  | GameOver -> let again = end_keyboard () in if again then main () else () 

(** [start_game f] loads the json from file [f] and makes the first call to 
    the main game loop.*)
and start_game f = let game = f |>  Yojson.Basic.from_file |> parse in 
  run_game game (init_state game) 

(** [main ()] prompts the user to choose a file of blocks to load, makes the 
    window, and starts the game engine.*)
and main () = 
  ANSITerminal.(print_string [red] "\n\nWelcome to OCaml Tetris.\n");
  print_endline "Please enter the name of the Tetris mode you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
    make_window ();
    start_game file_name
(* make_window ();
   start_game "tetris.json";*)

let () = main ()