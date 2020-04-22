open Graphics
open Board
open Game

type t 

val init_state : Game.t -> t

val blockref_x : t -> int

val blockref_y : t -> int

val add_blockref : t -> int -> int -> int * int

val won : t -> bool

val render_moving : t -> unit

val update : t -> Game.t -> t

val rotate : string -> int -> unit