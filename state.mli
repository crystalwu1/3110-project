open Graphics
open Board
open Game

type t 

val init_state : Game.t -> t

val blockref_x : t -> int

val blockref_y : t -> int

val won : t -> bool

val update : Game.t -> t -> t

val rotate : string -> t-> t

val move : string -> t -> t

val drop : t -> t

(** [row_remove st] is the updated state of [st] with the full rows
    removed *)
val row_remove : t -> t

