open Yojson.Basic.Util

type orientation_name = string

type shape_name = string

(** The type of a coordinate *)
type coordinate = {
  x : int;
  y : int 
}

(** The type of an orientation *)
type orientation = {
  orient_name : orientation_name;
  coordinates : coordinate list
}

(** The type of a shape *)
type shape = {
  shape_name : shape_name;
  color : int;
  orientations : orientation list;
}

type t = {
  shapes : shape list
}

(** [coordinates_of_json] is a record of ao coordinate from [j] *)
let coordinates_of_json j = {
  x = j |> member "x" |> to_int;
  y = j |> member "y" |> to_int;
}

(** [orientation_of_json] is a record of an orientation from [j] *)
let orientation_of_json j = {
  orient_name = j |> member "oname" |> to_string;
  coordinates = j |> member "coordinates" |> to_list |> List.map coordinates_of_json;
}

(** [shape_of_json] is a record of a shape from [j] *)
let shape_of_json j = {
  shape_name = j |> member "name" |> to_string;
  color = j |> member "color" |> to_string |> int_of_string;
  orientations = j |> member "orientations" |> to_list |> List.map orientation_of_json;
}

(** [game_of_json] is a record of a tetris game from [j] *)
let game_of_json j = {
  shapes = j |> member "tiles" |> to_list |> List.map shape_of_json;
}

let parse j =
  try game_of_json j 
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let shape_color shpe = 
  match shpe with 
  | None -> 0
  | Some thing -> thing.color

let shape_orientations shpe = 
  match shpe with 
  | None -> []
  | Some thing -> thing.orientations

let orientation_coordinates orient =
  match orient with 
  | None -> []
  | Some thing -> thing.coordinates

let coord_x coord = coord.x

let coord_y coord = coord.y

(** [rand_helper] is the [shape] in [shapes] at index [idx]. 
    Raises error if [idx] is too larger to index into [shapes] *)
let rec rand_helper shapes idx = 
  match shapes, idx with
  | h::t, x when x = 0 -> h
  | h::t, _ -> rand_helper t (idx-1)
  | [], _ -> failwith ("error with random shape indexing")

let rand_shape t = 
  let shapes = t.shapes in
  let length = List.length shapes in
  let idx = Random.int length in
  rand_helper shapes idx

let orientation_init shpe = 
  match shpe.orientations with
  | [] -> None
  | h::t -> Some h

