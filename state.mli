open Graphics
open Board
open Game

type t 

val init_state : Game.t -> t

val blockref_x : t -> int

val blockref_y : t -> int

val won : t -> bool

val update : t -> Game.t -> t

val rotate : string -> t-> t

val move : string -> t -> t