open Yojson.Basic.Util

type coordinate = (int * int)

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

let orientation_of_json j = {
  orient_name = j |> member "";
}