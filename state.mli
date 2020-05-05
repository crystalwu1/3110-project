open Graphics
open Board
open Game

exception GameWon

type t 

val init_state : Game.t -> t

val blockref_x : t -> int

val blockref_y : t -> int

val won : t -> bool

val update : Game.t -> t -> t

val hold : t -> t

val rotate : string -> t -> Game.t -> t

val move : string -> t -> t

val drop : t -> t

(** [row_remove st] is the updated state of [st] with the full rows
    removed *)
val row_remove : t -> t

