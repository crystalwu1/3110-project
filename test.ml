open OUnit2
open Yojson.Basic.Util
open Game
open Main
open State
open Board

let j = Yojson.Basic.from_file "tetris.json" |> parse
let shapes = j.shapes
let first_shape = (fun (h::t) -> h) shapes
let shp_name = first_shape.shp_name
let shp_color = first_shape.color
let shp_oris = first_shape.orientations
let fst_ori = (fun (h::t) -> h) first_shape.orientation
let fst_oname = fst_ori.orient_name
let fst_coor = (fun (h::t) -> h) fst_ori.coordinates
let fst_x = fst_coor.x
let fst_y = fst_coor.y

let game_tests = [
  "check color of first shape" >:: 
  (fun _ -> assert_equal (int_of_string("0x0F9AD7")) shp_color);
  "check name of first shape" >:: (fun _ -> assert_equal "long" shp_name);
  "check name of first shape's first orientation name" >:: 
  (fun _ -> assert_equal "orientation1" fst_oname);
  "check first x coordinate" >:: (fun _ -> assert_equal (-2) fst_x);
  "check first y coordinate" >:: (fun _ -> assert_equal 1 fst_y);
]

let command_tests = [

]

let simple_game = "test.json" |>  Yojson.Basic.from_file |> parse
let st = update simple_game (init_state simple_game 1)


let state_tests = [
  "check that update on new game makes a new moving block" >::
  (fun _ -> not(assert_equal (None) (st.moving_block)));
  "check that on another call to update does not change the moving block" >::
  (fun _ -> (assert_equal (st.moving_block) 
               (let new_st = update simple_game st in new_st.moving_block)));
  "test won on new game" >:: (fun _ -> (assert_equal (false) (won st)));
  "test that time is incrementing" >:: (fun _ -> not(assert_equal (0) (st.time)));

]


let suite =
  "Tetris test suite"  >::: List.flatten [
    game_tests;
    command_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite