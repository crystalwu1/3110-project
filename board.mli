(** Representation of the Tetris board and window.*)

(** The x-coordinate of the lower left corner of the Tetris board. *)
val startx : int

(** The y-coordinate of the lower left corner of the Tetris board. *)
val starty : int

(** The width of the Tetris board. *)
val boardw : int

(** The height of the Tetris board. *)
val boardh : int

(** The size of the sub-block that create Tetris pieces. *)
val tilesize : int

(** [make_window] is a graphical window with a Tetris board in it.*)
val make_window : unit -> unit