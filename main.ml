open Graphics
open Board
open Game
open State
open Command

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let rec run_game game st  = 
  (* print_endline "running"; *)
  try let x = keyboard game st in x |> update game |> run_game game with 
  | NoKeyPress -> run_game game (update game st)
  | GameOver -> let again = end_keyboard () in if again then main () else () 

and start_game f = let game = f |>  Yojson.Basic.from_file |> parse in 
  run_game game (init_state game) 

and main () = 
  ANSITerminal.(print_string [red] "\n\nWelcome to OCaml Tetris.\n");
  print_endline "Please enter the name of the Tetris mode you want to play.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
    make_window ();
    start_game file_name;
    read_helper ()
(* make_window ();
   start_game "standard.json";
   read_helper() *)

let () = main ()