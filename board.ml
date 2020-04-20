open Graphics

let startx = 50
let starty = 100
let boardw = 300
let boardh = 600
let black = rgb 0 0 0
let darkgrey = rgb 40 40 40
let lightgrey = rgb 100 100 100
let tilesize = boardh / 20

let clear_window color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ())

let rec grid_helper acc total constx consty dx dy=
  if acc = total then (moveto startx starty) else
    (rmoveto dx dy;
     rlineto constx consty;
     rmoveto dx dy;
     rlineto (-constx) (-consty);
     grid_helper (acc+1) total constx consty dx dy)

let create_board () = 
  moveto startx starty;
  set_color darkgrey;
  grid_helper 0 5 0 boardh tilesize 0;
  grid_helper 0 10 boardw 0 0 tilesize;

  set_color lightgrey;
  lineto startx (starty+boardh);
  lineto (startx+boardw) (starty+boardh);
  lineto (startx+boardw) starty;
  lineto startx starty

let make_background () = 
  clear_window black;
  create_board ();