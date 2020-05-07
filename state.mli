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

val init_state : Game.t -> int -> t

(** [won st] is [true] when the game has been won, otherwise [false].*)
val won : t -> bool

(** [update game st] is [st] altered to handle the animation of the block 
    moving down, removing filled rows, and creating the shadow of the block.*)
val update : Game.t -> t -> t

val hold : t -> t

val rotate : string -> t -> Game.t -> t

val move : string -> t -> t

(** [drop st] is [st] altered with the currently moving block dropped into the 
    array of dropped blocks.*)
val drop : t -> t

(** [row_remove st] is the updated state of [st] with the full rows
    removed *)
val row_remove : t -> t

val soft_drop : t -> t

