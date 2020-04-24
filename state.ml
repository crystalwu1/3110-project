open Graphics
open Board
open Game

let darkgrey = rgb 40 40 40
let orig_blockref = (200, 670)

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
  animate = 0;
  rows_left = 40; 
  (* ^ we can chane this to a user input at some poitn *)
}

let blockref_x st = 
  match st.blockref with
  | (x, _) -> x

let blockref_y st = 
  match st.blockref with
  | (_, y) -> y

let add_blockref st num1 num2 =
  match st.blockref with
  | (x, y) -> (x+num1, y+num2)

let won st = st.won

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

let erase_moving st = 
  let refx = blockref_x st in
  let refy = blockref_y st in 
  let color = shape_color st.moving_block in
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

let render_time st =
  set_color white;
  set_text_size 30;
  moveto 400 600;
  draw_string (string_of_int st.time)

let erase_time st = 
  fill_rect 400 600 30 100

let update st adv= 
  let result = 
    if st.moving_block = None then 
      let new_shape = rand_shape adv in 
      {
        blockref = st.blockref;
        moving_block = Some new_shape;
        current_orientation = orientation_init new_shape;
        time = st.time;
        queue = st.queue;
        won = st.won;
        dropped = st.dropped;
        animate = 0;
        rows_left = st.rows_left;
      }
    else (

      {
        blockref = if st.animate mod 10000 = 0 then add_blockref st 0 (-tilesize) else st.blockref;
        moving_block = st.moving_block;
        current_orientation = st.current_orientation;
        time = st.time;
        queue = st.queue;
        won = st.won;
        dropped= st.dropped;
        animate = if st.animate mod 10000 = 0 then 0 else st.animate +1;
        rows_left = st.rows_left;
      }) in 
  if st.animate mod 10000  = 0 then erase_moving st;
  if st.animate mod 7500 = 0 then erase_time st;
  render_time result;
  render_moving result; 
  result

(* changes the orientations of the moving_block *)
let rotate string st = 
  if string = "clockwise" then 
    let new_shape = {
      blockref = add_blockref st 0 0;
      moving_block = st.moving_block;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped= st.dropped;
      animate = st.animate;
      rows_left = st.rows_left;
      current_orientation = 
        let rec next_orientation list = 
          match list with 
          | [] -> failwith "no orientation"
          | h::[] -> if Some h = st.current_orientation 
            then Some (List.hd (list)) else failwith "no orientation"
          | h::t -> if Some h = st.current_orientation 
            then Some (List.hd (t)) else next_orientation t in
        next_orientation (shape_orientations (st.moving_block));
    } in new_shape 
  else 
    let new_shape = {
      blockref = add_blockref st 0 0;
      moving_block = st.moving_block;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped= st.dropped;
      animate = st.animate;
      rows_left = st.rows_left;
      current_orientation = 
        let rec next_orientation list = 
          match list with 
          | [] -> failwith "no orientation"
          | h::[] -> if Some h = st.current_orientation 
            then Some (List.nth list 3) else failwith "no orientation"
          | h::t -> if Some h = st.current_orientation 
            then Some (List.hd (List.rev t)) else next_orientation t in
        next_orientation (shape_orientations (st.moving_block));} in new_shape

let rec scan_width st orient acc=
  match orient with
  | [] -> acc
  | h::t -> (((blockref_x st) - (blockref_x st) mod 10)/10+(coord_x h))

let drop st = 
  let temp_state = {
    blockref = add_blockref st 0 0;
    moving_block = None;
    current_orientation = None;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped= st.dropped;
    animate = st.animate;
    rows_left = st.rows_left }
  in
  {
    blockref = orig_blockref;
    moving_block = None;
    current_orientation = None;
    time = st.time;
    queue = st.queue;
    won = st.won;
    dropped= st.dropped;
    animate = st.animate;
    rows_left = st.rows_left }

let move direction st =
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
      rows_left = st.rows_left } in new_shape
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
      rows_left = st.rows_left } in new_shape 

(** [find_lowest_y_helper] finds the index of the top most element 
    in [column] at [idx] and lower with a value greater than 0 *)
let rec find_lowest_y_helper column idx = 
  match column.(idx) with
  | n when n > 0 -> idx
  | _ -> find_lowest_y_helper column (idx-1)

let find_lowest_y dropped column =
  find_lowest_y_helper dropped.(column) 19

(** [row_check] returns true if the entire row starting at position [x] 
    at height [y] is nonzero and false otherwise *)
let rec row_check dropped y x =
  if dropped.(x).(y) > 0 then 
    if x = 9 then true
    else row_check dropped y (x+1)

(** [make_val] sets the element at position ([x], [y]) in the 
    array [dropped] to [value] *)
let make_val dropped y x value = 
  dropped.(x).(y) <- value

(** [rem_row] *)
let rec rem_row dropped y x =
  if y = 19 then
    if x = 9 then make_val dropped y x 0
    else make_val dropped y x 0; rem_row dropped y (x+1)
else 
if x = 9 then make_val dropped y x dropped.(x).(y+1); rem_row dropped (y+1) 0
else make_val dropped y x dropped.(x).(y+1); rem_row dropped y (x+1)

let rec row_remove_helper rows dropped pos = 
  5

let row_remove st =
  let new_rows_left = st.rows_left in
  let new_dropped = st.dropped in
  let result = row_remove_helper new_rows_left new_dropped 0 in
