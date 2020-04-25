open Graphics

let startx = 50
let starty = 100
let boardw = 300
let boardh = 600
let darkgrey = rgb 40 40 40
let white = rgb 255 255 255 
let tilesize = boardh / 20

(** [score_and_time color] writes "Lines Remaining:" and "Time:" in [color] .*)
let score_and_time color = 
  moveto 410 620;
  set_color color;
  draw_string ("Lines Remaining:");
  moveto 410 600;
  draw_string ("Score:")

(** [clear_window color] fills in the background of the window with [color] .*)
let clear_window color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ())

(** [grid_helper acc total constx consty dx dy] draws in [total] pairs of 
    grid lines of length [consty] and width [constx], with spacing width [dx] 
    and height [dy].*)
let rec grid_helper acc total constx consty dx dy=
  if acc = total then (moveto startx starty) else
    (rmoveto dx dy;
     rlineto constx consty;
     rmoveto dx dy;
     rlineto (-constx) (-consty);
     grid_helper (acc+1) total constx consty dx dy)

(** [create_board ()] draws the tetris board and grid.*)
let create_board () = 
  moveto startx starty;
  set_color darkgrey;
  grid_helper 0 5 0 boardh tilesize 0;
  grid_helper 0 10 boardw 0 0 tilesize;

  set_color darkgrey;
  lineto startx (starty+boardh);
  lineto (startx+boardw) (starty+boardh);
  lineto (startx+boardw) starty;
  lineto startx starty

let directions () = 
  failwith ""

let make_window () = 
  open_graph " 600x800";
  set_window_title "tetris";
  clear_window black;
  create_board ();
  score_and_time white