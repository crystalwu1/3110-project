open Graphics
open Board

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let main () = 
  make_window ();
  read_helper ()

let () = main ()