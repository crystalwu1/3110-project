open Graphics
open Board
open Game

type t = {
  blockref : int * int; 
  moving_block : Game.shape option;
  current_orientation : Game.orientation option;
  blocks : Game.coordinate list;
  time : int;
  queue : Game.shape list;
  won : bool;
  dropped : (int array) array;
}

let rec init_q length acc t = 
  match length with 
  | n when n = 0 -> acc
  | _ -> init_q (length-1) ((rand_shape t)::acc) t

let init_state t = {
  blockref = (200, 700);
  moving_block = None;
  current_orientation = None;
  blocks = [];
  time = 0;
  queue = init_q 5 [] t;
  won = false;
  dropped = Array.make_matrix 10 20 0
}

let blockref_x st = 
  match st.blockref with
  | (x, _) -> x

let blockref_y st = 
  match st.blockref with
  | (_, y) -> y

let add_blockref st num1 num2 =
  match st.blockref with
  | (x, y) -> (x+num1, x+num2)

let won st = st.won

(* renders the moving block *)
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
      draw_rect (refx+x*tilesize) (refy+y*tilesize) tilesize tilesize;
      helper t
  in set_color color; helper (orientation_coordinates st.current_orientation)

let update st adv= 
  if st.moving_block = None then 
    (* choose a random block and orientation  *)
    let new_shape = rand_shape adv in 
    {
      blockref = st.blockref;
      moving_block = Some new_shape;
      current_orientation = orientation_init new_shape;
      blocks = st.blocks;
      time = st.time;
      queue = st.queue;
      won = st.won;
      dropped = st.dropped;
    }
  else 
    (* check the decremting constant? *)
    (* move the block down *)
    (render_moving t;
     {
       blockref = add_blockref st 0 tilesize tilesize;
       moving_block = st.moving_block;
       current_orientation = st.current_orientation;
       blocks = st.blocks;
       time = st.time;
       queue = st.queue;
       won = st.won;
       dropped= st.dropped;
     })

(* idea: when the block hits the ground and is no longer the moving_block, 
   deconstruct the block into separate squares *)

(* how are we going to check the state of the board? like how will we 
   know when the cells are filled? *)

(* changes the orientations of the moving_block *)
let rotate string int = 
  failwith ""

let drop = 
  failwith ""

let move =
  failwith ""

let find_lowest_y dropped x =
