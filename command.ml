open Graphics
open Board
open Game
open State

let status = wait_next_event[Poll] 


    if status.key_pressed then 
      match read_key with 
      | 'd' -> move "right"
      | 'a' -> move "left"
      | 'w' -> rotate "clockwise" 90
      | 's' | ' ' -> drop else 



