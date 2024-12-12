type t = {
  mutable x_cord : int;
  mutable y_cord : int;
  mutable width : int;
  mutable height : int;
      (* store additional information about player in future sprints (e.g.
         inventory) *)
}

let create_player x y w h = { x_cord = x; y_cord = y; width = w; height = h }

let move_player player dx dy =
  player.x_cord <- player.x_cord + dx;
  player.y_cord <- player.y_cord + dy

let current_x_pos player = player.x_cord
let current_y_pos player = player.y_cord
let get_width player = player.width
let get_height player = player.height

let get_corners obj =
  {
    x_cord = current_x_pos obj;
    y_cord = current_y_pos obj;
    height = current_x_pos obj + get_height obj;
    width = current_y_pos obj + get_width obj;
  }

let player_shoot player projectiles_ref direction =
  let dx, dy = Direction.to_player_projectile_delta direction in
  let new_projectile =
    Projectile.create_proj
      (current_x_pos player + (get_width player / 2))
      (current_y_pos player + (get_height player / 2))
      dx dy
  in
  projectiles_ref := new_projectile :: !projectiles_ref
