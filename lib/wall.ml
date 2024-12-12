type t = {
  x : int;
  y : int;
  width : int;
  height : int;
}

let create_wall x y width height = { x; y; width; height }
let get_wall_position w = (w.x, w.y)
let get_wall_size w = (w.width, w.height)

let in_wall_bounds w width height =
  w.x >= 0 && w.x + w.width <= width && w.y >= 0 && w.y + w.height <= height
