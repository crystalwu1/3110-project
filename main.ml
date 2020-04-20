open Graphics

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let main () = 
  Graphics.open_graph "";
  set_window_title "tetris";
  read_helper ()

let () = main ()