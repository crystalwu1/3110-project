open OUnit2
open Yojson.Basic.Util
open Game
(* open Main *)
open State
open Board

let head_of lst cond =   
  match lst, cond with 
  | h::t, false -> h 
  | h::h2::t, true -> h2
  | _ -> failwith "test error" 

let j = Yojson.Basic.from_file "tetris.json" |> parse
let shapes = get_shapes j
let first_shape = head_of shapes false
let shp_name = get_shape_name first_shape
let shp_color = shape_color (Some first_shape)
let shp_oris = shape_orientations (Some first_shape)
let fst_ori = head_of shp_oris false
let fst_oname = orientation_name (Some fst_ori)
let coors = orientation_coordinates (Some fst_ori)
let fst_coor = head_of coors false
let fst_x = coord_x fst_coor
let fst_y = coord_y fst_coor
let init_orient = orientation_init first_shape
let height = shape_height first_shape
let next_orientatio = 
  next_orientation "clockwise" j (Some first_shape) (Some fst_ori)
let expected_next_orientation = Some (head_of shp_oris true)

let pj = Yojson.Basic.from_file "pentris.json" |> parse
let pshapes = get_shapes pj
let pfirst_shape = head_of pshapes false
let pshp_name = get_shape_name pfirst_shape
let pshp_color = shape_color (Some pfirst_shape)
let pshp_oris = shape_orientations (Some pfirst_shape)
let pfst_ori = head_of pshp_oris false
let pfst_oname = orientation_name (Some pfst_ori)
let pcoors = orientation_coordinates (Some pfst_ori)
let pfst_coor = head_of pcoors false
let pfst_x = coord_x pfst_coor
let pfst_y = coord_y pfst_coor
let pinit_orient = orientation_init pfirst_shape
let pheight = shape_height pfirst_shape
let pnext_orientatio = 
  next_orientation "clockwise" pj (Some pfirst_shape) (Some pfst_ori)
let pexpected_next_orientation = Some (head_of pshp_oris true)


let game_tests = [
  "check color of first shape in tetris" >:: 
  (fun _ -> assert_equal (int_of_string("0x0F9AD7")) shp_color);
  "check name of first shape in tetris" >:: (fun _ -> assert_equal "long" shp_name);
  "check name of first shape's first orientation name in tetris" >:: 
  (fun _ -> assert_equal "orientation1" fst_oname);
  "check first x coordinate in tetris" >:: (fun _ -> assert_equal (-2) fst_x);
  "check first y coordinate in tetris" >:: (fun _ -> assert_equal 1 fst_y);
  "check orientation initialization in tetris" >:: 
  (fun _ -> assert_equal init_orient (Some fst_ori));
  "check shape height in tetris" >:: (fun _ -> assert_equal height 1);
  "check next orientation in tetris" >:: 
  (fun _ -> assert_equal next_orientatio expected_next_orientation);
  "check color of first shape in pentris" >:: 
  (fun _ -> assert_equal (int_of_string("0x0F9AD7")) pshp_color);
  "check name of first shape in pentris" >:: 
  (fun _ -> assert_equal "long" pshp_name);
  "check name of first shape's first orientation name in pentris" >:: 
  (fun _ -> assert_equal "left" pfst_oname);
  "check first x coordinate in pentris" >:: (fun _ -> assert_equal 0 pfst_x);
  "check first y coordinate in pentris" >:: (fun _ -> assert_equal 2 pfst_y);
  "check orientation initialization in pentris" >:: 
  (fun _ -> assert_equal pinit_orient (Some pfst_ori));
  "check shape height in pentris" >:: (fun _ -> assert_equal pheight 5);
  "check next orientation in pentris" >:: 
  (fun _ -> assert_equal pnext_orientatio pexpected_next_orientation);
]

let command_tests = [

]

(* let simple_game = "test.json" |>  Yojson.Basic.from_file |> parse
   let st = update simple_game (init_state simple_game 1) *)


let state_tests = [
  (* "check that update on new game makes a new moving block" >::
     (fun _ -> not(assert_equal (None) (st.moving_block)));
     "check that on another call to update does not change the moving block" >::
     (fun _ -> (assert_equal (st.moving_block) 
               (let new_st = update simple_game st in new_st.moving_block)));
     "test won on new game" >:: (fun _ -> (assert_equal (false) (won st)));
     "test that time is incrementing" >:: (fun _ -> not(assert_equal (0) (st.time))); *)

]


let suite =
  "Tetris test suite"  >::: List.flatten [
    game_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite