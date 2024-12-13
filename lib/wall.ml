type t = {
  x : int;
  y : int;
  width : int;
  height : int;
}

let create_wall x y width height = { x; y; width; height }
let wall_x_pos wall = wall.x
let wall_y_pos wall = wall.y
let get_wall_width wall = wall.width
let get_wall_height wall = wall.height

let in_wall_bounds wall width height =
  wall.x >= 0
  && wall.x + wall.width <= width
  && wall.y >= 0
  && wall.y + wall.height <= height
