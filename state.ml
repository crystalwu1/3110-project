open Graphics
open Board
open Game

let darkgrey = rgb 40 40 40
let orig_blockref = (200, 670)

exception NoMoreBlocks

type t = {
  blockref : int * int; 
  moving_block : Game.shape option;
  current_orientation : Game.orientation option;
  time : int;
  queue : Game.shape list;
  won : bool;
  dropped : (int array) array;
  animate : int;
  rows_left : int;
}

(** [init_q length acc t] initializes a [Game.shape list] of random shapes. *)
let rec init_q length acc t = 
  match length with 
  | n when n = 0 -> acc
  | _ -> init_q (length-1) ((rand_shape t)::acc) t

let init_state t = {
  blockref = orig_blockref;
  moving_block = None;
  current_orientation = None;
  time = 0;
  queue = init_q 5 [] t;
  won = false;
  dropped = Array.make_matrix 10 20 0;
  animate = 1;
  rows_left = 40; 
  (* ^ we can chane this to a user input at some point *)
}

let blockref_x st = 
  match st.blockref with
  | (x, _) -> x

let blockref_y st = 
  match st.blockref with
  | (_, y) -> y

(** [convert_blk_to_pix_coor st lst acc] is a list of pixel coordinates 
    calculated from the [blockref] value in [st] and the block coordinates
    from [lst] combined with the pixel coordinates in [acc] *)
let rec convert_blk_to_pix_coor st lst acc = 
  match lst with
  | [] -> acc
  | h::t -> convert_blk_to_pix_coor st t 
              (((blockref_x st)+((coord_x h)*tilesize),
                (blockref_y st)+((coord_y h)*tilesize))::acc)

(* (blockref+(coord value *tilesize ) *)

(** [add-blockref st num1 num2] is an [(int * int)] containing the current blockref of [st], but with
    [num1] added to the first value, and [num2] added to the second.*)
let add_blockref st num1 num2 =
  match st.blockref with
  | (x, y) -> (x+num1, y+num2)


let won st = st.won
(** [render_moving] draws the current moving_block in [st].*)
let render_moving st = 
  let refx = blockref_x st in
  let refy = blockref_y st in 
  let color = shape_color st.moving_block in
  let rec helper coords =
    match coords with
    | [] -> ()
    | h::t -> 
      let x = coord_x h in
      let y = coord_y h in
      fill_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
      helper t
  in 
  set_color color; helper (orientation_coordinates st.current_orientation)

(** [render_array] draws the dropped blocks in [st].*)
let rec render_array dropped x y = 
  if y > 19 
  then () 
  else (
    if dropped.(x).(y) = 0 
    then 
      if (x=9 && y=19) 
      then ()
      else 
      if y=19 
      then render_array dropped (x+1) 0
      else (render_array dropped x (y+1))
    else 
    if (x=9 && y=19) 
    then fill_rect (x*tilesize+50) (y*tilesize+100) tilesize tilesize
    else 
    if y=19 
    then (set_color dropped.(x).(y); fill_rect (x*tilesize+50) (y*tilesize+100) tilesize tilesize; render_array dropped (x+1) 0)
    else (set_color dropped.(x).(y); fill_rect (x*tilesize+50) (y*tilesize+100) tilesize tilesize; render_array dropped x (y+1)
         ))

(** [erase_moving] redraws the background grid in the shape of the currently moving block in [st].*)
let erase_moving st = 
  let refx = blockref_x st in
  let refy = blockref_y st in 
  let rec helper coords =
    match coords with
    | [] -> ()
    | h::t -> 
      let x = coord_x h in
      let y = coord_y h in
      set_color black;
      fill_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
      set_color darkgrey;
      draw_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
      helper t
  in set_color darkgrey; helper (orientation_coordinates st.current_orientation)

(** [render_time st] draws the rows remaining from [st] into the board.*)
let render_lines_remaining st =
  set_color white;
  set_text_size 30;
  moveto 490 600;
  draw_string (string_of_int st.rows_left)

(** [erase_lines_remaining st] redraws the window over the rows remaining. *)
let erase_lines_remaining st = 
  set_color black;
  fill_rect 490 600 30 100 

(** [render_time st] draws the time integer into the board.*)
let render_time st =
  set_color white;
  set_text_size 30;
  moveto 450 600;
  draw_string (string_of_int st.time)

(** [erase_time st] redraws the window over the time. *)
let erase_time st = 
  set_color black;
  fill_rect 450 600 30 100  

let pop queue adv = 
  match queue with 
  | x::t -> (x, (rand_shape adv :: t))
  | [] -> raise NoMoreBlocks

(** [find_lowest_y_helper] finds the index of the top most element 
    in [column] at [idx] and lower with a value greater than 0 *)
let rec find_lowest_y_helper column idx = 
  if idx < 0 then 0 else 
    match column.(idx) with
    | n when n > 0 -> (idx)
    | _ -> find_lowest_y_helper column (idx-1)

(** finds the highest cell that is filled*)
let find_lowest_y dropped column =
  find_lowest_y_helper dropped.(column) 19

let rec scan_width st orient acc=
  match orient with
  | [] -> acc  
  | h::t -> let col = ((blockref_x st)-50)/tilesize+(coord_x h) in
    (* print_endline (string_of_int col); *)
    let calc = find_lowest_y st.dropped col in
    print_endline (string_of_int calc);
    match acc with
    | x -> scan_width st t (if calc > x then calc else acc)


(** [add_coordinate dropped x y color] set coordinate ([x],[y]) to [color] *)
let add_coordinate dropped x y color = 
  dropped.(x).(y) <- color

(** [add_dropped_block dropped x y coor_list color] will add the value [color] 
    to dropped at the coordinates from [coor_list] relative to [x] and [y]  *)
let rec add_dropped_block dropped x y low coor_list color =
  match coor_list with 
  | [] -> ()
  | h::t -> let x_coor = coord_x h in 
    let y_coor = coord_y h in 
    add_coordinate dropped (x+x_coor) ((-low)+y+y_coor) color;
    add_dropped_block dropped x y low t color

let bottom_coord lst acc= 
  match lst with 
  | [] -> acc 
  | h::t -> if (coord_y h) < acc then coord_y h else acc

(** [can_remove dropped y x] is true if there is no 0 valued 
    element in row [y] starting at position [x] and false otherwise *)
let rec can_remove dropped y x =
  if dropped.(x).(y) > 0 then 
    if x = 9 then true
    else can_remove dropped y (x+1)
  else false

(** [make_val dropped y x valu] sets the element at position 
    ([x], [y]) in the array [dropped] to [value] *)
let make_val dropped y x value = 
  dropped.(x).(y) <- value

(** [rem_row dropped y x] will replace all values beginning at ([x],[y]) 
    with the value at position ([x],[y+1]) to remove the row [y]. 
    Values with positions will be initialised to 0 if their [y] 
    cooridnate is 19. *)
let rec rem_row dropped y x =
  if y = 19 then 
    if x = 9 then make_val dropped y x 0
    else (make_val dropped y x 0; rem_row dropped y (x+1))
  else 
  if x = 9 then (make_val dropped y x dropped.(x).(y+1); 
                 rem_row dropped (y+1) 0)
  else make_val dropped y x dropped.(x).(y+1); rem_row dropped y (x+1)

(** [row_remove_helper dropped pos rows_removed] will remove the full 
    rows in [dropped] starting at the y coordinate [pos] and returns the 
    number of rows that have been removed in addition to [rows_removed] *)
let rec row_remove_helper dropped pos rows_removed = 
  if (can_remove dropped pos 0) 
  then (rem_row dropped pos 0; 
        row_remove_helper dropped (pos+1) (rows_removed+1))
  else row_remove_helper dropped (pos+1) (rows_removed)

let row_remove st =
  let new_rows_removed = row_remove_helper st.dropped 0 0 in
  {
    blockref = st.blockref; 
    moving_block = st.moving_block;
    current_orientation = st.current_orientation;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left - new_rows_removed;
  }

let update game st = 
  let result = 
    if st.moving_block = None then 
      let (new_shape, new_queue) = pop st.queue game in 
      {
        blockref = st.blockref;
        moving_block = Some new_shape;
        current_orientation = orientation_init new_shape;
        time = st.time;
        queue = new_queue;
        won = st.won;
        dropped = st.dropped;
        animate = st.animate;
        rows_left = st.rows_left;
      }
    else (
      {
        blockref = if st.animate mod 100 = 0 then add_blockref st 0 (-tilesize) else st.blockref;
        moving_block = st.moving_block;
        current_orientation = st.current_orientation;
        time = st.time;
        queue = st.queue;
        won = st.won;
        dropped= st.dropped;
        animate = if st.animate mod 100 = 0 then 1 else st.animate +1;
        rows_left = st.rows_left;
      }) in 
  if st.animate mod 100 = 0 then erase_moving st;
  (* print_endline (string_of_bool (st.animate=100)); *)
  (* if st.animate mod 7500 = 0 then erase_time st; *)
  render_time result;
  render_moving result; 
  result

let rotate string st game = 
  let new_shape = {
    blockref = add_blockref st 0 0;
    moving_block = st.moving_block;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
    current_orientation = 
      next_orientation string game st.moving_block st.current_orientation
  }
  in erase_moving st; new_shape

let rec leftmost_coord acc lst = 
  match lst with
  | [] -> acc
  | (x,y)::t -> if x < acc 
    then leftmost_coord x t else leftmost_coord acc t

let rec rightmost_coord acc lst = 
  match lst with
  | [] -> acc
  | (x,y)::t -> if x > acc 
    then rightmost_coord x t else rightmost_coord acc t

let move direction st =
  let  pixel_list = convert_blk_to_pix_coor st (orientation_coordinates st.current_orientation) [] in 

  if ((leftmost_coord (blockref_x st) (pixel_list)) <= 50 
      && direction = "left") then st
  else 
<<<<<<< HEAD
  if (rightmost_coord (blockref_x st) (pixel_list)) >= 350 
=======
  if (rightmost_coord (blockref_x st) (pixel_list)) >= 350 - tilesize  
>>>>>>> 82ae776e1dab0fadf3eaf3d38dd15dd74402adfe
  && direction = "right" then st 
  else 
  if direction = "right" then 
    let new_shape = {
      blockref = add_blockref st tilesize 0;
      moving_block = st.moving_block;
      current_orientation = st.current_orientation;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped= st.dropped;
      animate = st.animate;
      rows_left = st.rows_left } in erase_moving st; new_shape
  else  
    let new_shape = {
      blockref = add_blockref st (-tilesize) 0;
      moving_block = st.moving_block;
      current_orientation = st.current_orientation;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
      animate = st.animate;
      rows_left = st.rows_left } in erase_moving st; new_shape 

let drop st = 
  let coords = orientation_coordinates st.current_orientation in
  let x = (((blockref_x st)-50)/tilesize) in
  let y = (scan_width st coords 0) in
  let color = shape_color st.moving_block in
  (* print_endline (string_of_int y); *)
  (* print_endline (string_of_int (bottom_coord coords 0)); *)
  add_dropped_block st.dropped x (y) (bottom_coord coords 0) coords color;
  erase_moving st;
  render_array st.dropped 0 0;
  print_endline "safe";
  {
    blockref = orig_blockref;
    moving_block = None;
    current_orientation = None;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped= st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
  }