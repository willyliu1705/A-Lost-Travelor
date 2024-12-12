type t = {
  x : int;
  y : int;
  dx : int;
  dy : int;
}

let player_projectiles = ref []
let enemy_projectiles = ref []
let create_proj x y dx dy = { x; y; dx; dy }
let move_proj p = { p with x = p.x + p.dx; y = p.y + p.dy }

let in_bounds p width height =
  p.x >= 0 && p.x < width && p.y >= 0 && p.y < height

let get_proj_position p = (p.x, p.y)

let collision_helper proj x y w h =
  let px, py = get_proj_position proj in
  px >= x && px <= x + w && py >= y && py <= y + h

let detect_collision projectiles_ref x y w h =
  List.exists (fun proj -> collision_helper proj x y w h) !projectiles_ref

let handle_collision projectiles_ref x y w h =
  projectiles_ref :=
    List.filter
      (fun proj -> not (collision_helper proj x y w h))
      !projectiles_ref

let clear_all_projectiles () =
  player_projectiles := [];
  enemy_projectiles := []
