open Graphics
open Board
open Game
open State
open Command

exception InvalidFile
exception InvalidLineNum

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let rec run_game game st  = 
  (* print_endline "running"; *)
  try let x = keyboard game st in x |> update game |> run_game game with 
  | NoKeyPress -> run_game game (update game st)
  | GameOver -> let again = end_keyboard () in if again then main () else () 

and start_game f lines = let game = f |>  Yojson.Basic.from_file |> parse in 
  run_game game (init_state game lines) 

and main () = 
  ANSITerminal.(print_string [red] "\n\nWelcome to OCaml Tetris.\n");
  print_endline "Please enter the name of the Tetris mode you want to play.\n";
  print_string  "> ";
  let file_name =
    match read_line () with
    | exception End_of_file -> raise InvalidFile
    | read_name -> read_name in
  print_endline "Please enter the number of lines to win a game.\n";
  print_string  "> ";
  let lines_to_win =
    match read_line () with
    | exception End_of_file -> raise InvalidLineNum
    | lines -> int_of_string lines in
  make_window ();
  start_game file_name lines_to_win;
  read_helper ()
(* make_window ();
   start_game "tetris.json" 40;
   read_helper() *)

let () = main ()