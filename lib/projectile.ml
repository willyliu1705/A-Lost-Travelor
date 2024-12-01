open Graphics

type t = {
  x : int;
  y : int;
  dx : int;
  dy : int;
}

let create_projectile x y dx dy = { x; y; dx; dy }

let move_all projectiles =
  let move p = { p with x = p.x + p.dx; y = p.y + p.dy } in
  let in_bounds p =
    p.x < size_x () && p.x >= 0 && p.y < size_y () && p.y >= 0
  in
  List.filter in_bounds (List.map move projectiles)

let draw_all projectiles =
  List.iter (fun p -> draw_rect p.x p.y 5 5) projectiles
