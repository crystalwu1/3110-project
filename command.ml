open Graphics
open Board
open Game
open State

exception NoKeyPress

let d = Char.chr 37

let keyboard game st =
  let status = wait_next_event [Poll] in 
  if status.keypressed then
    (match read_key () with 
    | 'd' | '/' -> move "right" st
    | 'a' | ',' -> move "left" st
    | 'w' | 'l' -> rotate "clockwise" st game
    | 'x' -> rotate "counterclockwise" st game
    | 's' | ' ' -> drop st
    | _ -> raise NoKeyPress)
  else raise NoKeyPress




