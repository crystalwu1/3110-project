(** 
   Representation of dynamic game state.

   This module represents the current state of the Tetris game, including the 
   currently moving block, the queue of future blocks, and the blocks that have
    already been placed.
*)

(** The exception that is thrown when the game has been won: i.e. when there 
    are 0 lines remaining.*)
exception GameWon

(** The abstract type of values representing the game state. *)
type t 

(** [init_state t lines] is the initial state of the game [t] where 
    [lines] is the numbers of lines needed to win  *)
val init_state : Game.t -> int -> t

(** [moving_block st] is the currently moving block in the game.*)
val moving_block : t -> Game.shape option

(** [hold st] is the current hold block in the game.*)
val hold : t -> Game.shape option

(** [current_orientation st] is the current orientation of the moving block
    in the game.*)
val current_orientation : t -> Game.orientation option

(** [time_st st] is the recorded time in [st]*)
val time_st : t -> int

(** [rows_left st] is the number of rows left to clear in [st]*)
val rows_left : t -> int

(** [queue st] is the queue of next blocks in [st]*)
val queue : t -> Game.shape list

(** [won st] is [true] when the game has been won, otherwise [false].*)
val won : t -> bool

(** [blockref_y st] is the y-coordinate of the render point of the currently
    moving block in [st]. *)
val blockref_y : t -> int

(** [blockref_x st] is the x-coordinate of the render point of the currently
    moving block in [st]. *)
val blockref_x : t -> int

(** [update game st] is [st] altered to handle the animation of the block 
    moving down, removing filled rows, and creating the shadow of the block
    given the information in [game] *)
val update : Game.t -> t -> t

val hold : t -> t

val rotate : string -> t -> Game.t -> t

val move : string -> t -> t

(** [drop st] is [st] altered with the currently moving block dropped into the 
    array of dropped blocks.*)
val drop : t -> t

(** [row_remove st] is the updated state of [st] with the full rows removed *)
val row_remove : t -> t

val soft_drop : t -> t

