open Yojson.Basic.Util

type coordinate = {
  x : int;
  y : int 
}

type orientation = {
  orient_name : string;
  coordinates : coordinate list
}

type shape = {
  shape_name : string;
  color : int;
  orientations : orientation list;
}

type t = {
  shapes : shape list
}

let coordinates_of_json j = {
  x = j |> member "x" |> to_int;
  y = j |> member "y" |> to_int;
}

let orientation_of_json j = {
  orient_name = j |> member "oname" |> to_string;
  coordinates = j |> member "coordinates" |> List.map coordinates_of_json;
}

let shape_color shpe = 
  failwith "Rachel todo"

let shape_orientations shpe = 
  failwith "Rachel todo"

