open Graphics
open Board
open Game

type t = {
  blockref : Game.coordinate; 
  moving_block : Game.shape;
  current_orientation : Game.coordinate;
  blocks : Game.coordinate list;
  time : int;
}

let blockref_x st = 
  match st.blockref with
  | (x, _) -> x

let blockref_y st = 
  match st.blockref with
  | (_, y) -> y

(* renders the moving block *)
let render_moving blk st= 
  let refx = blockref_x st in
  let refy = blockref_y st in 
  let color = shape_color st.moving_block in 
  let rec helper coords =
    match coords with
    | [] -> ()
    | (x, y)::t -> 
      moveto (refx+(x*tilesize)) (refy+(y*tilesize));
      rlineto tilesize 0;
      rlineto 0 tilesize;
      rlineto (-tilesize) 0;
      rlineto 0 (-tilesize);
      helper t
  in set_color color; helper 
    (* is this ok or should we have more getters? *)
    (st.moving_block |> shape_orientations |> List.map (fun x -> x.coordinates))

(* idea: when the block hits the ground and is no longer the moving_block, 
   deconstruct the block into separate squares *)

(* how are we going to check the state of the board? like how will we 
   know when the cells are filled? *)

(* changes the orientations of the moving_block *)
let rotate = 
  failwith ""