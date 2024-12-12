open Graphics
open Dungeon_crawler.Player
open Dungeon_crawler.Projectile
open Dungeon_crawler.Direction
open Dungeon_crawler.Enemy

type keyword = { mutable word : string }

let keyword = { word = "Start Menu" }
let player1 = create_player 40 40 10 10
(* player has to be sufficiently small or else weird interactions will occur
   with the enemy entities (e.g. enemy sees the player "faster" when entering
   the enemy's line of sight from the right compared to the left; as a result,
   this causes the enemy to not shoot at the player when its supposed to. ) *)

let player_direction = ref right (* Change player default direction here *)

(* Enemies should be the same size or larger than the player to prevent
   unintended interactions and misalignment issues for enemy line of sight
   between the enemy and the player (an example is described above). *)
let enemy1 = create_enemy 50 50 50 50 up 1 1.0
let enemy2 = create_enemy 10 10 10 10 right 1 2.0
let enemy3 = create_enemy 10 30 10 10 right 1 0.5

let draw_player player =
  draw_rect (current_x_pos player) (current_y_pos player) (get_width player)
    (get_height player)

let draw_enemy enemy =
  let x = enemy_x_pos enemy in
  let y = enemy_y_pos enemy in
  let w = get_enemy_width enemy in
  let h = get_enemy_height enemy in
  set_color blue;
  draw_rect x y w h;
  set_color black

let draw_projectiles projectiles =
  List.iter
    (fun p ->
      let x, y = get_proj_position p in
      draw_circle x y 3)
    projectiles

let move_projectiles projectiles =
  let screen_width = size_x () in
  let screen_height = size_y () in
  projectiles |> List.map move_proj
  |> List.filter (fun p -> in_bounds p screen_width screen_height)

let player_shoot player projectiles_ref direction =
  let dx, dy = to_player_projectile_delta direction in
  let new_projectile =
    create_proj
      (current_x_pos player + (get_width player / 2))
      (current_y_pos player + (get_height player / 2))
      dx dy
  in
  projectiles_ref := new_projectile :: !projectiles_ref

let update_enemy enemy =
  if aligned_with_player enemy (current_x_pos player1, current_y_pos player1)
  then
    let delay = get_shooting_delay enemy in
    enemy_shoot enemy enemy_projectiles enemy_last_shot_time delay
      (Unix.gettimeofday ())

let update_player player =
  if key_pressed () then
    match read_key () with
    | key -> (
        match of_key key with
        | Some dir ->
            player_direction := dir;
            let dx, dy = to_player_delta dir in
            move_player player dx dy
        | None ->
            if key = ' ' then
              player_shoot player player_projectiles !player_direction)

let handle_projectile_collision_with_enemy projectiles_ref enemy =
  let ex = enemy_x_pos enemy in
  let ey = enemy_y_pos enemy in
  let ew = get_enemy_width enemy in
  let eh = get_enemy_height enemy in
  handle_collision projectiles_ref ex ey ew eh

let handle_enemy_projectiles_with_player enemy_projectiles player =
  let px = current_x_pos player in
  let py = current_y_pos player in
  let pw = get_width player in
  let ph = get_height player in
  if detect_collision enemy_projectiles px py pw ph then
    handle_collision enemy_projectiles px py pw ph

let draw_enemies () =
  draw_enemy enemy1;
  draw_enemy enemy2;
  draw_enemy enemy3

let update_enemies () =
  update_enemy enemy1;
  update_enemy enemy2;
  update_enemy enemy3

let check_press_start player =
  let mouse_position = mouse_pos () in
  if button_down () then
    match mouse_position with
    | x, y ->
        if x >= 766 && x <= 1141 && y >= 133 && y <= 253 then
          let () = clear_graph () in
          keyword.word <- "Game Start"
        else ();
        synchronize ()

(** [draw_rect_centered] draws the rectangle centered at point [x], [y] with
    width [w] and height [h].*)
let draw_rect_centered x y w h = draw_rect (x - (w / 2)) (y - (h / 2)) w h

let draw_screens keyword =
  match keyword with
  | "Start Menu" ->
      set_color black;
      draw_rect 0 0 1907 986;
      fill_rect 0 0 1907 986;
      set_color white;
      draw_poly_line [| (1354, 986); (1354, 0) |];
      draw_poly_line [| (554, 986); (554, 0) |];
      draw_circle 954 483 10;
      draw_rect_centered 954 193 375 120;
      moveto 913 193;
      draw_string "PRESS TO START";
      draw_arc 954 800 400 100 0 180;
      draw_arc 954 200 400 100 180 360;
      check_press_start ()
  | "Game Start" ->
      set_color red;
      draw_rect 0 0 1907 986;
      draw_player player1;
      draw_enemies ();
      update_player player1;
      update_enemies ();
      draw_projectiles !player_projectiles;
      draw_projectiles !enemy_projectiles;
      player_projectiles := move_projectiles !player_projectiles;
      enemy_projectiles := move_projectiles !enemy_projectiles;
      handle_projectile_collision_with_enemy player_projectiles enemy1;
      handle_projectile_collision_with_enemy player_projectiles enemy2;
      handle_projectile_collision_with_enemy player_projectiles enemy3;
      handle_enemy_projectiles_with_player enemy_projectiles player1;
      synchronize ()
  | _ -> ()

let () =
  open_graph "";
  while true do
    auto_synchronize false;
    open_graph " 1908x987";
    draw_screens keyword.word
  done
