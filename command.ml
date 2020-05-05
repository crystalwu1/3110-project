open Graphics
open Board
open Game
open State
open Unix

exception NoKeyPress

let noncanon () =
  let termio = Unix.tcgetattr Unix.stdin in
  let newtermio = {termio with c_icanon = false; c_vmin = 1; c_vtime = 0; c_echo = false}
  in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW newtermio;
  at_exit (fun () -> Unix.tcsetattr Unix.stdin Unix.TCSANOW termio)

let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSANOW
      { termio with Unix.c_icanon = false } in
  let res = input_char Stdlib.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW termio;
  res

let handle_escape st game =
  match input_char Stdlib.stdin with
  (* ESC *)
  | '[' -> begin 
      match input_char Stdlib.stdin with
      | 'A' -> rotate "clockwise" st game
      (* | 'B' -> print_endline "Down" *)
      | 'C' -> move "right" st
      | 'D' -> move "left" st
      | _ -> raise NoKeyPress
    end
  | _ -> raise NoKeyPress

let keyboard2 game st =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSANOW
      { termio with Unix.c_icanon = false } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW termio;
  (match input_char Stdlib.stdin with 
   | '\027' -> handle_escape st game
   | 'd' ->  move "right" st
   | 'a' | ',' -> move "left" st
   | 'w' | '\033' -> rotate "clockwise" st game
   | 'x' -> rotate "counterclockwise" st game
   | 's' | ' ' -> drop st
   | 'c' -> hold st
   | _ -> raise NoKeyPress)

let keyboard game st =
  let status = wait_next_event [Poll] in 
  if status.keypressed then
    (match read_key () with 
     | 'd' ->  move "right" st
     | 'a' | ',' -> move "left" st
     | 'w' | '\033' -> rotate "clockwise" st game
     | 'x' -> rotate "counterclockwise" st game
     | 's' | ' ' -> drop st
     | 'c' -> hold st
     | _ -> raise NoKeyPress)
  else raise NoKeyPress

let end_keyboard () =
  print_endline "in end";
  let status = wait_next_event [Poll] in 
  if status.keypressed then
    (match read_key () with 
     | 'y' -> true
     | 'n' -> false
     | _ -> raise NoKeyPress)
  else raise NoKeyPress




