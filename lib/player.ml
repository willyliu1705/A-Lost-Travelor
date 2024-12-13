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

let get_corners obj =
  {
    x_cord = current_x_pos obj;
    y_cord = current_y_pos obj;
    height = current_x_pos obj + get_height obj;
    width = current_y_pos obj + get_width obj;
    hp = get_hp obj;
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

let handle_enemy_projectiles_with_player enemy_projectiles player =
  let px = current_x_pos player in
  let py = current_y_pos player in
  let pw = get_width player in
  let ph = get_height player in
  let collided = ref false in
  enemy_projectiles :=
    List.filter
      (fun proj ->
        let collision =
          Projectile.detect_collision enemy_projectiles px py pw ph
        in
        if collision then collided := true;
        not collision)
      !enemy_projectiles;
  if !collided then change_hp player (-10)
