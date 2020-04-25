open Graphics
open Board
open Game
open State

exception NoKeyPress

let keyboard game st =
  let status = wait_next_event [Poll] in 
  if status.keypressed then 
    match read_key () with 
    | 'd' -> move "right" st
    | 'a' -> move "left" st
    | 'w' -> rotate game "clockwise" st 
    | 'x' -> rotate game "counterclockwise" st 
    | 's' | ' ' -> drop st
    | _ -> raise NoKeyPress
  else raise NoKeyPress




