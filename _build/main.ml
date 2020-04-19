open Graphics

let rec read_helper () =
  match read_line () with
  | exception End_of_file -> ()
  | _ -> ()

let main () = 
  Graphics.open_graph "";
  read_helper ()

let () = main ()