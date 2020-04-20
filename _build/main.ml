open Graphics
open Board

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let main () = 
  open_graph " 500x800";
  set_window_title "tetris";
  make_background ();
  read_helper ()

let () = main ()