type t = {
  mutable x_cord : int;
  mutable y_cord : int;
  mutable width : int;
  mutable height : int;
  mutable hp : int;
}

let create_player x y w h =
  { x_cord = x; y_cord = y; width = w; height = h; hp = 100 }

let move_player player dx dy =
  player.x_cord <- player.x_cord + dx;
  player.y_cord <- player.y_cord + dy

let move_player_absolute player x y =
  player.x_cord <- x;
  player.y_cord <- y

let current_x_pos player = player.x_cord
let current_y_pos player = player.y_cord
let get_width player = player.width
let get_height player = player.height
let get_hp player = player.hp
let last_heal_time = ref 15.0

let change_hp player amount =
  if amount < 0 then player.hp <- max 0 (player.hp + amount)
  else if player.hp < 100 then
    let new_hp = min 100 (player.hp + amount) in
    player.hp <- new_hp

let player_shoot player projectiles_ref direction =
  let dx, dy = Direction.to_player_projectile_delta direction in
  let new_projectile =
    Projectile.create_proj
      (current_x_pos player + (get_width player / 2))
      (current_y_pos player + (get_height player / 2))
      dx dy
  in
  projectiles_ref := new_projectile :: !projectiles_ref

let handle_enemy_projectiles_with_player projectiles_ref player =
  let player_x = current_x_pos player in
  let player_y = current_y_pos player in
  let player_w = get_width player in
  let player_h = get_height player in
  projectiles_ref :=
    List.filter
      (fun proj ->
        let proj_x, proj_y = Projectile.get_proj_position proj in
        let collision =
          proj_x >= player_x
          && proj_x <= player_x + player_w
          && proj_y >= player_y
          && proj_y <= player_y + player_h
        in
        if collision then change_hp player (-10);
        not collision)
      !projectiles_ref
