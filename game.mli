(** 
   Representation of tetris game settings. This module will parse data stored
   in the JSON files, load it, and allow querying. The data is information 
   in game setting such as tile shape, color, and possible rotating positions.
*)

type coordinate

type shape

type orientation

(** type of orientation names *)
type orientation_name = string

(** type of shape names *)
type shape_name = string

(** abstract type of values representing a tetris game. *)
type t

(** [parse j] is a record of a tetirs game from [j]
    Raises [Type_error] when [j] is a wrong type of json element] *)
val parse : Yojson.Basic.t -> t

val shape_color : shape option -> int

val orientation_name : orientation option -> string

val shape_orientations : shape option -> orientation list

val orientation_coordinates : orientation option -> coordinate list 

val coord_x : coordinate -> int

val coord_y : coordinate -> int

(** [rand_shape] is a random [shape] in tetris game [t] *)
val rand_shape : t -> shape

val orientation_init : shape -> orientation option

val shape_height : shape -> int

val next_orientation : string -> t -> shape option -> 
  orientation option -> orientation option