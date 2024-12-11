open Dungeon_crawler.Projectile
open Dungeon_crawler.Direction
open Dungeon_crawler.Enemy

type keyword = { mutable word : string }

let keyword = { word = "Start Menu" }
let player1 = create_player 40 40 50 50
let player_projectiles = ref []
let enemy_projectiles = ref []
let player_direction = ref right (* Change player default direction here *)
let enemy = create_enemy 50 50 50 50 up
let enemy_last_shot_time = ref 0.0
let enemy_shoot_delay = 1.5 (* Change enemy shooting delay here *)

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
      draw_rect x y 5 5)
    projectiles

let move_projectiles projectiles =
  let screen_width = size_x () in
  let screen_height = size_y () in
  projectiles |> List.map move_proj
  |> List.filter (fun p -> in_bounds p screen_width screen_height)

let player_shoot player projectiles_ref direction =
  let dx, dy = to_projectile_delta direction in
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
    enemy_shoot enemy enemy_projectiles enemy_last_shot_time enemy_shoot_delay
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
      draw_enemy enemy;
      update_player player1;
      update_enemy enemy;
      draw_projectiles !player_projectiles;
      draw_projectiles !enemy_projectiles;
      player_projectiles := move_projectiles !player_projectiles;
      enemy_projectiles := move_projectiles !enemy_projectiles;
      synchronize ()
  | _ -> ()

let () =
  open_graph "";
  while true do
    auto_synchronize false;
    open_graph " 1908x987";
    draw_screens keyword.word
  done

(* let get_corners player = {}

   let collision_check (obj1 : Dungeon_crawler.Player.t) (obj2:
   Dungeon_crawler.Player.t) dx dy = let corners_obj1 = get_corners obj1 in let
   corners_obj2 = get_corners obj2 in if corners_obj1 *)
