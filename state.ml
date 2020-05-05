open Graphics
open Board
open Game

let darkgrey = rgb 40 40 40
let orig_blockref = (startx + (boardw/2), starty + boardh -tilesize)

exception NoMoreBlocks
exception GameWon

type t = {
  blockref : int * int; 
  moving_block : Game.shape option;
  hold : Game.shape option; 
  current_orientation : Game.orientation option;
  time : int;
  queue : Game.shape list;
  won : bool;
  dropped : (int array) array;
  animate : float;
  rows_left : int;
}
(** [init_q length acc t] initializes a [Game.shape list] of random shapes. *)
let rec init_q length acc t = 
  match length with 
  | n when n = 0 -> acc
  | _ -> init_q (length-1) ((rand_shape t)::acc) t

(** [start] is a reference to the time that the game has started *)
let start = ref (Unix.time ())

(** [time ()] is the amount of time in seconds since the game has started. 
    This function will also update the time on the game board. *)
let time () =
  set_color black;
  fill_rect 540 680 100 10;  
  set_color white;
  set_text_size 30;
  moveto 550 680; 
  let time = int_of_float((Unix.time () -. !start)) in
  draw_string (string_of_int time);
  time

let init_state t = 
  start := (Unix.time ());
  {
    blockref = orig_blockref;
    moving_block = None;
    hold = None;
    current_orientation = None;
    time = time ();
    queue = init_q 5 [] t;
    won = false;
    dropped = Array.make_matrix 10 20 0;
    animate = Unix.time ();
    rows_left = 1; 
    (* ^ we can chane this to a user input at some point *)
  }
let blockref_x st = 
  match st.blockref with
  | (x, _) -> x

let blockref_y st = 
  match st.blockref with
  | (_, y) -> y

let curr_row st =
  (blockref_y st - starty) / tilesize

let curr_col st = 
  let pixels = (blockref_x st)- startx in
  let calc = pixels - (pixels mod tilesize) in (calc/tilesize)

let coord_to_pix axis num = 
  if axis = "x" 
  then startx + (num * tilesize)
  else starty + (num * tilesize)

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

(** [render_block] draws the current moving_block in [st].*)
let render_block mov_block refx refy color orientation =  
  let rec helper coords =
    match coords with
    | [] -> ()
    | h::t -> 
      let x = coord_x h in
      let y = coord_y h in
      if (refy + y * tilesize) < 700 then
        (fill_rect (refx + x * tilesize) (refy + y * tilesize) tilesize tilesize;
         helper t)
      else 
        helper t
  in 
  set_color color; helper (orientation_coordinates orientation)

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
    then fill_rect (x*tilesize+startx) (y*tilesize+starty) tilesize tilesize
    else 
    if y=19 
    then (set_color dropped.(x).(y); fill_rect (x*tilesize+startx) (y*tilesize+starty) tilesize tilesize; render_array dropped (x+1) 0)
    else (set_color dropped.(x).(y); fill_rect (x*tilesize+startx) (y*tilesize+starty) tilesize tilesize; render_array dropped x (y+1)
         ))

(** [erase_block] redraws the background grid in the shape of the currently 
    moving block in [st] at [refx, refy].*)
let erase_block st refx refy orientation = 
  let rec helper coords =
    match coords with
    | [] -> ()
    | h::t -> 
      let x = coord_x h in
      let y = coord_y h in
      if (refy+y*tilesize) < (starty + boardh) then
        (set_color black;
         fill_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
         set_color darkgrey;
         draw_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
         helper t)
      else helper t
  in set_color darkgrey; helper (orientation_coordinates orientation)


(** [erase_lines_remaining st] redraws the window over the rows remaining. *)
let erase_lines_remaining () = 
  set_color black;
  fill_rect 610 700 30 100 

(** [render_lines_remaining st] draws the rows remaining from [st] into the board.*)
let render_lines_remaining num =
  erase_lines_remaining ();
  set_color white;
  set_text_size 30;
  moveto 610 700;
  draw_string (string_of_int num);
  num

let erase_q () = 
  set_color black;
  fill_rect 460 0 200 650

let rec render_q q dx dy =
  match q with
  | [] -> ()
  | h::t -> render_block (Some h) dx dy (shape_color (Some h)) (orientation_init h); 
    render_q t dx (dy-(tilesize*(1+(shape_height h))))

let pop queue game = 
  match queue with 
  | x::t -> let q = (t@(rand_shape game::[])) in 
    erase_q () ; render_q q 540 570; (x, q)
  | [] -> raise NoMoreBlocks

(** [find_lowest_y_helper] finds the index of the top most element 
    in [column] at [idx] and lower with a value greater than 0 *)
let rec find_lowest_y_helper column idx = 
  if idx < 0 then -1 else 
    match column.(idx) with
    | n when n > 0 -> (idx)
    | _ -> find_lowest_y_helper column (idx-1)

(** finds the highest cell that is filled*)
let find_lowest_y dropped column =
  find_lowest_y_helper dropped.(column) 19

(** [add_coordinate dropped x y color] set coordinate ([x],[y]) to [color] *)
let add_coordinate dropped x y color = 
  dropped.(x).(y) <- color

(** [can_remove dropped y x] is true if there is no 0 valued 
    element in row [y] starting at position [x] and false otherwise *)
let rec can_remove dropped y x =
  if dropped.(x).(y) > 0 then 
    if x = 9 then (true, dropped)
    else can_remove dropped y (x+1)
  else (false, dropped)

(** [make_val dropped y x valu] sets the element at position 
    ([x], [y]) in the array [dropped] to [value] *)
let make_val dropped y x value = 
  dropped.(x).(y) <- value;
  dropped

(** [rem_row dropped y x] will replace all values beginning at ([x],[y]) 
    with the value at position ([x],[y+1]) to remove the row [y]. 
    Values with positions will be initialised to 0 if their [y] 
    cooridnate is 19. *)
let rec rem_row dropped y x =
  if y = 19 then 
    if x = 9 then make_val dropped y x 0
    else let dr = make_val dropped y x 0 in rem_row dr y (x+1)
  else 
  if x = 9 then let dr2 = make_val dropped y x dropped.(x).(y+1) in
    rem_row dr2 (y+1) 0
  else let dr3 = make_val dropped y x dropped.(x).(y+1) in 
    rem_row dr3 y (x+1)

(** [row_remove_helper dropped pos rows_removed] will remove the full 
    rows in [dropped] starting at the y coordinate [pos] and returns the 
    number of rows that have been removed in addition to [rows_removed] *)
let rec row_remove_helper dropped pos rows_removed = 
  if pos < 20 then
    match (can_remove dropped pos 0) with
    | (true, dr) -> 
      let dr2 = (rem_row dr pos 0) in
      row_remove_helper dr2 (pos+1) (rows_removed+1)
    | (false, dr) -> row_remove_helper dr (pos+1) (rows_removed)
  else (rows_removed, dropped)

let row_remove st =
  let (new_rows_removed, drop) = row_remove_helper st.dropped 0 0 in
  {
    blockref = st.blockref; 
    moving_block = st.moving_block;
    hold = st.hold; 
    current_orientation = st.current_orientation;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = drop;
    animate = st.animate;
    rows_left = st.rows_left - new_rows_removed;
  }

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

let hold st = 
  erase_block st (blockref_x st) (blockref_y st) st.current_orientation; 
  set_color black;
  fill_rect 0 480 50 50;
  render_block st.moving_block 25 500 (shape_color st.moving_block) st.current_orientation;
  let new_current_shape = {
    blockref = add_blockref st 0 0;
    moving_block = st.hold;
    hold = st.moving_block;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
    current_orientation = 
      match st.hold with 
      | Some h -> orientation_init h
      | None -> None}  in 
  render_block (new_current_shape.moving_block)  (blockref_x new_current_shape)
    (blockref_y new_current_shape) (shape_color new_current_shape.moving_block) new_current_shape.current_orientation;
  new_current_shape

let won st = st.won  

let display_win_message time =
  set_color black;
  fill_rect 0 0 700 800;
  set_color white;
  moveto 190 370;
  draw_string ("Congratulations! You win! You time is " ^ (string_of_int time) ^ ". Nice job.");
  moveto 260 350;
  draw_string ("Press 'y' to play again.")


let win time rows_left = 
  if rows_left > 0
  then rows_left
  else (display_win_message time; rows_left)


(* returns (y-value of the highest cell within the block's width, y-coordinate of that block) *)
let rec parse_dropped dropped coords curr_col acc= 
  match coords with 
  | [] -> acc
  | h::t -> 
    let x = coord_x h in 
    let y = coord_y h in 
    let temp_col = curr_col + x in
    let temp = find_lowest_y dropped temp_col in 
    (* print_endline (string_of_int temp); *)
    let updated = 
      if temp = (fst acc) 
      then
        if y > (snd acc)
        then acc 
        else (temp, y)
      else 
      if temp > (fst acc)
      then (temp, y) 
      else acc in 
    parse_dropped dropped t curr_col updated

let rec add_to_dropped dropped color coords target_cell y_target_coord curr_col= 
  match coords with 
  | [] -> ()
  | h::t -> 
    let x = coord_x h in 
    let y = coord_y h in 
    add_coordinate dropped (curr_col+x) (target_cell-y_target_coord+y) color;
    add_to_dropped dropped color t target_cell y_target_coord curr_col

let rec update_after_row_rem drop x y  = 
  set_color drop.(x).(y);
  fill_rect (x*tilesize+startx) (y*tilesize+starty) tilesize tilesize;
  if x > 8 then 
    if y > 18 then () else update_after_row_rem drop 0 (y+1)
  else update_after_row_rem drop (x+1) y

let drop st = 
  let color = shape_color st.moving_block in
  let coords = orientation_coordinates st.current_orientation in
  let curr_col = curr_col st in 
  let (target_cell, y_target_coord) = parse_dropped st.dropped coords curr_col (-4, -4) in
  print_endline "-------------";
  print_endline (string_of_int curr_col);
  print_endline (string_of_int target_cell);
  print_endline (string_of_int y_target_coord);
  add_to_dropped st.dropped color coords (target_cell+1) y_target_coord curr_col;
  erase_block st (blockref_x st) (blockref_y st) st.current_orientation;
  render_array st.dropped 0 0;
  let new_st = 
    {
      blockref = orig_blockref;
      moving_block = None;
      hold = st.hold;
      current_orientation = None;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped= st.dropped;
      animate = st.animate;
      rows_left = render_lines_remaining st.rows_left;
    } in
  let final_res = row_remove new_st in
  update_after_row_rem final_res.dropped 0 0;
  if (win final_res.time final_res.rows_left) > 0 then final_res else raise GameWon

let rotate string st game = 
  let new_shape = {
    blockref = add_blockref st 0 0;
    moving_block = st.moving_block;
    hold = st.hold;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped = st.dropped;
    animate = st.animate;
    rows_left = st.rows_left;
    current_orientation = 
      next_orientation string game st.moving_block st.current_orientation
  } in 
  let pixel_list = convert_blk_to_pix_coor st (orientation_coordinates new_shape.current_orientation) [] in 
  if (leftmost_coord (blockref_x st) pixel_list) <= startx then  
    let shifted_right_shape = {
      blockref = add_blockref st tilesize 0;
      moving_block = st.moving_block;
      hold = st.hold;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
      animate = st.animate;
      rows_left = st.rows_left;
      current_orientation = 
        next_orientation string game st.moving_block st.current_orientation } 
    in erase_block st (blockref_x st) (blockref_y st) st.current_orientation; shifted_right_shape
  else 
  if (rightmost_coord (blockref_x st) pixel_list) >= 350 - tilesize then 
    let shifted_left_shape = {
      blockref = add_blockref st (-tilesize) 0;
      moving_block = st.moving_block;
      hold = st.hold;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
      animate = st.animate;
      rows_left = st.rows_left;
      current_orientation = 
        next_orientation string game st.moving_block st.current_orientation } 
    in erase_block st (blockref_x st) (blockref_y st) st.current_orientation; shifted_left_shape
  else  let new_shape = {
      blockref = add_blockref st 0 0;
      moving_block = st.moving_block;
      hold = st.hold;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
      animate = st.animate;
      rows_left = st.rows_left;
      current_orientation = 
        next_orientation string game st.moving_block st.current_orientation
    } in erase_block st (blockref_x st) (blockref_y st) st.current_orientation; new_shape


let move direction st =
  let  pixel_list = convert_blk_to_pix_coor st (orientation_coordinates st.current_orientation) [] in 
  if ((leftmost_coord (blockref_x st) (pixel_list)) <= startx 
      && direction = "left") then st
  else 
  if (rightmost_coord (blockref_x st) (pixel_list)) >= (startx+boardw) - tilesize  
  && direction = "right" then st 
  else 
  if direction = "right" then 
    let new_shape = {
      blockref = add_blockref st tilesize 0;
      moving_block = st.moving_block;
      hold = st.hold;
      current_orientation = st.current_orientation;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped= st.dropped;
      animate = st.animate;
      rows_left = st.rows_left } in 
    erase_block st (blockref_x st) (blockref_y st) st.current_orientation; 
    new_shape
  else  
    let new_shape = {
      blockref = add_blockref st (-tilesize) 0;
      moving_block = st.moving_block;
      hold = st.hold;
      current_orientation = st.current_orientation;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
      animate = st.animate;
      rows_left = st.rows_left } in erase_block st (blockref_x st) (blockref_y st) st.current_orientation; new_shape

let update game st = 
  let final_res = row_remove st in
  update_after_row_rem final_res.dropped 0 0;
  let result = 
    if st.moving_block = None then 
      let (new_shape, new_queue) = pop st.queue game in 
      {
        blockref = st.blockref;
        moving_block = Some new_shape;
        hold = st.hold;
        current_orientation = orientation_init new_shape;
        time = time ();
        queue = new_queue;
        won = st.won;
        dropped = st.dropped;
        animate = st.animate;
        rows_left = render_lines_remaining final_res.rows_left;
      }
    else (
      {
        blockref = if (Unix.time ()) -. st.animate = 1. then add_blockref st 0 (-tilesize) else st.blockref;
        moving_block = st.moving_block;
        hold = st.hold;
        current_orientation = st.current_orientation;
        time = time ();
        queue = st.queue;
        won = (if st.rows_left = 0 then true else false);
        dropped = st.dropped;
        (* animate = if st.animate mod 100 = 0 then 1 else st.animate +1; *)
        animate = if (Unix.time ()) -. st.animate = 1. then (Unix.time ()) else st.animate;
        rows_left = render_lines_remaining final_res.rows_left;
      }) in 
  let coords = orientation_coordinates st.current_orientation in
  let curr_col = curr_col st in 
  let (target_cell, y_target_coord) = parse_dropped st.dropped coords curr_col (-4, -4) in
  if (curr_row st) - 1 + y_target_coord <= target_cell then drop result else
    (if (Unix.time ()) -. st.animate = 1. then erase_block st (blockref_x st) (blockref_y st) st.current_orientation;
     render_block result.moving_block (blockref_x st) (coord_to_pix "y" (target_cell + 1 - y_target_coord)) (darkgrey) st.current_orientation;
     render_block result.moving_block (blockref_x result) (blockref_y result) (shape_color st.moving_block) st.current_orientation; 
     create_board ();
     result)




