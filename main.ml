open Graphics
open Board
open Game
open State

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let rec run_game adv st = if won st then () else run_game adv (update st)

let start_game f = let adventure = f |>  Yojson.Basic.from_file |> parse in run_game

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
    make_window ();
    start_game file_name;
    read_helper ()

let () = main ()