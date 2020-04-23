open Graphics
open Board
open Game
open State

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let rec run_game st adv = 
  (* print_endline "running"; *)
  if won st then () 
  else run_game (update st adv) adv

let start_game f = let adventure = f |>  Yojson.Basic.from_file |> parse in 
  run_game (init_state adventure) adventure

let main () = 
  (* ANSITerminal.(print_string [red]
                  "\n\nWelcome to OCaml Tetris.\n");
  print_endline "Please enter the name of the Tetris mode you want to play.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
    make_window ();
    start_game file_name;
    read_helper () *)
    make_window ();
    start_game "standard.json";
    read_helper()

let () = main ()