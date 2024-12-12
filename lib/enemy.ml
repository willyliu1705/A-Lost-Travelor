type t = {
  x : int;
  y : int;
  width : int;
  height : int;
  mutable direction : Direction.t;
  projectile_speed : int;
  shooting_delay : float;
}

let enemy_last_shot_time = ref 0.0

let create_enemy x y w h dir projectile_speed shooting_delay =
  {
    x;
    y;
    width = w;
    height = h;
    direction = dir;
    projectile_speed;
    shooting_delay;
  }

let enemy_x_pos enemy = enemy.x
let enemy_y_pos enemy = enemy.y
let get_enemy_width enemy = enemy.width
let get_enemy_height enemy = enemy.height
let set_direction enemy dir = enemy.direction <- dir
let get_direction enemy = enemy.direction
let get_projectile_speed enemy = enemy.projectile_speed
let get_shooting_delay enemy = enemy.shooting_delay

let enemy_shoot enemy projectiles_ref last_shot_time delay current_time =
  if current_time -. !last_shot_time >= delay then (
    let dx, dy =
      if Direction.is_up (get_direction enemy) then
        (0, get_projectile_speed enemy)
      else if Direction.is_down (get_direction enemy) then
        (0, -get_projectile_speed enemy)
      else if Direction.is_left (get_direction enemy) then
        (-get_projectile_speed enemy, 0)
      else if Direction.is_right (get_direction enemy) then
        (get_projectile_speed enemy, 0)
      else ((0, 0) [@coverage off])
      (* should never be able to reach last statement since there are only four
         directions *)
    in
    let x = enemy_x_pos enemy in
    let y = enemy_y_pos enemy in
    let w = get_enemy_width enemy in
    let h = get_enemy_height enemy in
    let new_projectile =
      Projectile.create_proj (x + (w / 2)) (y + (h / 2)) dx dy
    in
    projectiles_ref := new_projectile :: !projectiles_ref;
    last_shot_time := current_time)

let aligned_with_player enemy (px, py) =
  let ex = enemy_x_pos enemy in
  let ey = enemy_y_pos enemy in
  let ew = get_enemy_width enemy in
  let eh = get_enemy_height enemy in
  match enemy.direction with
  | dir when dir = Direction.up -> px >= ex && px <= ex + ew && py > ey
  | dir when dir = Direction.down -> px >= ex && px <= ex + ew && py < ey
  | dir when dir = Direction.left -> py >= ey && py <= ey + eh && px < ex
  | dir when dir = Direction.right -> py >= ey && py <= ey + eh && px > ex
  | _ -> false [@coverage off]
(* should never be able to reach last branch since there are only four
   directions *)
