type t = {
  x : int;
  y : int;
  dx : int;
  dy : int;
}

let create_proj x y dx dy = { x; y; dx; dy }
let move_proj p = { p with x = p.x + p.dx; y = p.y + p.dy }

let in_bounds p width height =
  p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

let get_position p = (p.x, p.y)
