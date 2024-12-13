open Graphics
open Dungeon_crawler.Player
open Dungeon_crawler.Projectile
open Dungeon_crawler.Direction
open Dungeon_crawler.Enemy
open Dungeon_crawler.Wall

type keyword = { mutable word : string }

let keyword = { word = "Start Menu" }

type room_completed = { mutable completed : bool }

let room_completed = { completed = false }

type room_counter = { mutable room_counter : int }

let room_counter = { room_counter = 0 }

type list_of_enemies = {
  mutable list_of_enemies : Dungeon_crawler.Enemy.t list;
}

let list_of_enemies = { list_of_enemies = [] }
let brown = rgb 150 75 0
let gray = rgb 211 211 211

(* player has to be sufficiently small or else weird interactions will occur
   with the enemy entities (e.g. enemy sees the player "faster" when entering
   the enemy's line of sight from the right compared to the left; as a result,
   this causes the enemy to not shoot at the player when its supposed to. ) *)
let player1 = create_player 150 450 30 30

let walls =
  [
    Dungeon_crawler.Wall.create_wall 0 60 910 60;
    Dungeon_crawler.Wall.create_wall 1015 60 1810 60;
    Dungeon_crawler.Wall.create_wall 0 890 910 890;
    Dungeon_crawler.Wall.create_wall 1015 890 1810 890;
    Dungeon_crawler.Wall.create_wall 8 60 8 890;
    Dungeon_crawler.Wall.create_wall 1810 60 1810 400;
    Dungeon_crawler.Wall.create_wall 1810 540 1810 890;
  ]

let draw_rect_centered x y w h = draw_rect (x - (w / 2)) (y - (h / 2)) w h
let fill_rect_centered x y w h = fill_rect (x - (w / 2)) (y - (h / 2)) w h

let draw_player player =
  let () = set_color red in
  let () =
    draw_rect_centered (current_x_pos player) (current_y_pos player)
      (get_width player) (get_height player)
  in
  let () = set_color black in
  let () =
    draw_arc
      (current_x_pos player + (get_width player / 2))
      (current_y_pos player + (get_height player / 2))
      5 5 (-90) 180
  in
  let () = set_color red in
  if !player_direction = right then
    draw_poly_line
      [|
        ( current_x_pos player + (get_width player / 4),
          current_y_pos player - (get_height player / 4) );
        ( current_x_pos player + (get_width player / 4),
          current_y_pos player + (get_height player / 4) );
      |]
  else if !player_direction = left then
    draw_poly_line
      [|
        ( current_x_pos player - (get_width player / 4),
          current_y_pos player - (get_height player / 4) );
        ( current_x_pos player - (get_width player / 4),
          current_y_pos player + (get_height player / 4) );
      |]
  else if !player_direction = up then
    draw_poly_line
      [|
        ( current_x_pos player - (get_width player / 4),
          current_y_pos player + (get_height player / 4) );
        ( current_x_pos player + (get_width player / 4),
          current_y_pos player + (get_height player / 4) );
      |]
  else if !player_direction = down then
    draw_poly_line
      [|
        ( current_x_pos player - (get_width player / 4),
          current_y_pos player - (get_height player / 4) );
        ( current_x_pos player + (get_width player / 4),
          current_y_pos player - (get_height player / 4) );
      |]

let draw_hp_bar () =
  set_line_width 5;
  set_color black;
  draw_poly_line
    [| (30, 940); (30, 970); (30, 955); (50, 955); (50, 940); (50, 970) |];
  draw_poly_line [| (70, 940); (70, 970) |];
  draw_poly_line [| (70, 970); (85, 970) |];
  draw_poly_line [| (85, 968); (85, 957) |];
  draw_poly_line [| (70, 955); (85, 955) |];
  draw_rect 105 945 200 20;
  if get_hp player1 <= 0 then keyword.word <- "Game Over";
  set_color red;
  fill_rect 105 945 (get_hp player1 * 2) 20;
  moveto 130 950;
  set_color black;
  draw_string (string_of_int (get_hp player1) ^ "/100");
  set_line_width 2

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

let update_enemy enemy =
  if aligned_with_player enemy (current_x_pos player1, current_y_pos player1)
  then
    let delay = get_shooting_delay enemy in
    enemy_shoot enemy enemy_projectiles enemy_last_shot_time delay
      (Unix.gettimeofday ())

(* Collision detection between two rectangles *)
let rectangles_intersect (x1, y1, w1, h1) (x2, y2, w2, h2) =
  (* Check if there is overlap between the player’s rectangle and the wall’s
     rectangle *)
  x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

let move_player_no_collision player dx dy walls =
  let new_x = current_x_pos player + dx in
  let new_y = current_y_pos player + dy in

  (* Check for collision with walls *)
  if
    List.exists
      (fun wall ->
        let wall_x, wall_y = get_wall_position wall in
        let wall_w, wall_h = get_wall_size wall in
        (* Check if the player's new position collides with the wall *)
        let player_rect = (new_x, new_y, get_width player, get_height player) in
        let wall_rect = (wall_x, wall_y, wall_w, wall_h) in
        rectangles_intersect player_rect wall_rect)
      walls
  then () (* No movement if there is a collision *)
  else move_player player dx dy (* Allow movement if no collision *)

let update_player player walls =
  if key_pressed () then
    match read_key () with
    | key -> (
        match of_key key with
        | Some dir ->
            player_direction := dir;
            let dx, dy = to_player_delta dir in
            move_player_no_collision player dx dy
              walls (* Move with collision check *)
        | None ->
            if key = ' ' then
              let () =
                player_shoot player player_projectiles !player_direction
              in
              change_hp player (-1)
            else if key = 'q' then
              let current_time = Unix.gettimeofday () in
              if current_time -. !last_heal_time >= 15.0 then (
                change_hp player 5;
                last_heal_time := current_time))

let draw_heal_ability () =
  set_line_width 3;
  set_color white;
  fill_rect 90 27 41 45;
  set_color black;
  draw_rect_centered 110 50 32 8;
  draw_rect_centered 110 50 8 32;
  draw_rect_centered 110 50 45 50;
  set_color yellow;
  fill_rect_centered 110 50 32 8;
  fill_rect_centered 110 50 8 32;
  set_color gray;
  fill_rect 90 27 41
    (let time_elapsed = Unix.gettimeofday () -. !last_heal_time in
     if time_elapsed < 15. then (15 - int_of_float time_elapsed) * 3 else 0);
  moveto 93 31;
  set_color red;
  let time_elapsed = Unix.gettimeofday () -. !last_heal_time in
  if time_elapsed < 15. then
    draw_string (string_of_int (15 - int_of_float time_elapsed))
  else draw_string "Q"

let () = Random.self_init ()
let array_of_possible_directions = [| left; right; up; down |]

let create_random_enemy room_difficulty =
  create_enemy
    (Random.int 1657 + 100)
    (Random.int 736 + 100)
    (Random.int 10 + get_width player1)
    (Random.int 10 + get_height player1)
    array_of_possible_directions.(Random.int 4)
    (1. +. (float_of_int room_difficulty /. 10.))
    (let shooting_delay = 5. -. Random.float (float_of_int room_difficulty) in
     if shooting_delay >= 0.25 then shooting_delay else 0.25)

(* Enemies should be the same size or larger than the player to prevent
   unintended interactions and misalignment issues for enemy line of sight
   between the enemy and the player (an example is described above). *)
let enemy1 = create_random_enemy room_counter.room_counter
let enemy2 = create_random_enemy room_counter.room_counter
let enemy3 = create_random_enemy room_counter.room_counter

let draw_enemy enemy =
  let x = enemy_x_pos enemy in
  let y = enemy_y_pos enemy in
  let w = get_enemy_width enemy in
  let h = get_enemy_height enemy in
  set_color blue;
  draw_rect x y w h;
  set_color black

let draw_enemies () =
  draw_enemy enemy1;
  draw_enemy enemy2;
  draw_enemy enemy3

let update_enemies () =
  update_enemy enemy1;
  update_enemy enemy2;
  update_enemy enemy3

let draw_pressure_plate x y w h =
  set_color black;
  set_line_width 5;
  let () = draw_rect_centered x y w h in
  if
    (current_x_pos player1 >= x - (w / 2) - (get_width player1 / 2)
    && current_x_pos player1 <= x - (w / 2) + w + (get_width player1 / 2))
    && current_y_pos player1 >= y - (h / 2) - (get_height player1 / 2)
    && current_y_pos player1 <= y - (h / 2) + h + (get_height player1 / 2)
  then room_completed.completed <- true

let draw_button action next_room =
  set_color white;
  draw_rect_centered 954 193 375 120;
  moveto 913 193;
  draw_string ("PRESS TO " ^ action);
  let mouse_position = mouse_pos () in
  if button_down () then
    match mouse_position with
    | x, y ->
        if x >= 766 && x <= 1141 && y >= 133 && y <= 253 then
          keyword.word <- next_room
        else ();
        synchronize ()

let draw_start_menu () =
  set_color black;
  fill_rect 0 0 1908 987;

  Unix.sleepf 0.2;
  (* Add stars as background *)
  for _ = 1 to 350 do
    let x = Random.int 1908 in
    let y = Random.int 987 in
    let brightness = Random.int 255 in
    set_color (rgb brightness brightness brightness);
    fill_circle x y (Random.int 2)
  done;
  draw_button "START" "Tutorial Room 1";
  set_color white;
  moveto 913 793;
  draw_string "A Lost Traveler";
  synchronize ()

(* THESE ARE THE COORDINATES OF THE WALLS *)
let draw_normal_room_boundaries () =
  set_color gray;
  fill_rect 0 0 100 987;
  fill_rect 101 887 1706 100;
  fill_rect 1807 0 101 987;
  fill_rect 100 0 1706 100;
  if room_completed.completed then (
    set_color (rgb 118 209 247);
    fill_rect 903 886 101 101;
    fill_rect 1807 443 101 101;
    fill_rect 903 0 101 101);
  set_color black;
  set_line_width 10;
  draw_segments
    [|
      (100, 100, 100, 886);
      (100, 886, 903, 886);
      (1004, 886, 1807, 886);
      (1807, 886, 1807, 543);
      (1807, 443, 1807, 100);
      (1807, 100, 1004, 100);
      (903, 100, 100, 100);
    |]

let draw_open_room_doors () =
  set_line_width 10;
  set_color yellow;
  draw_segments
    [|
      (903, 886, 903, 987);
      (1004, 886, 1004, 987);
      (1807, 443, 1908, 443);
      (1807, 543, 1908, 543);
      (903, 100, 903, 0);
      (1004, 100, 1004, 0);
    |]

let draw_closed_room_doors () =
  set_line_width 10;
  set_color brown;
  draw_segments
    [| (903, 886, 1004, 886); (1807, 543, 1807, 443); (1004, 100, 903, 100) |]

let check_next_room next_room =
  if room_completed.completed then
    if
      current_x_pos player1 >= 903
      && current_x_pos player1 <= 1004
      && current_y_pos player1 <= 0
    then
      let () = keyword.word <- next_room in
      move_player_absolute player1 950 850
    else if
      current_x_pos player1 >= 903
      && current_x_pos player1 <= 1004
      && current_y_pos player1 >= 987
    then
      let () = keyword.word <- next_room in
      move_player_absolute player1 950 150
    else if
      current_x_pos player1 >= 1908
      && current_y_pos player1 >= 443
      && current_y_pos player1 <= 543
    then
      let () = keyword.word <- next_room in
      move_player_absolute player1 150 493

let draw_tutorial_room_1 () =
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () =
       draw_string "Great! Now walk through any of the doors that opened."
     in
     check_next_room "Tutorial Room 2"
   else
     let () = draw_closed_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     draw_string
       "Use WASD to move! Try to walk to the pressure plate in the middle.");
  set_line_width 2;
  set_color red;
  draw_player player1;
  draw_hp_bar ();
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  player_projectiles := move_projectiles !player_projectiles;
  draw_pressure_plate 954 493 50 50;
  synchronize ()

let draw_tutorial_room_2 () =
  room_completed.completed <- false;
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () =
       draw_string "Nice job! Complete 1 more room to finish the tutorial."
     in
     check_next_room "Tutorial Room 3"
   else
     let () = draw_closed_room_doors () in
     let () = set_color brown in
     let () = moveto 30 920 in
     let () =
       draw_string
         "You can press SPACEBAR to shoot projectiles. Every time you fire a \
          projectile though, you will lose HP. "
     in
     let () = moveto 30 900 in
     draw_string
       "Fire sparingly! You can also press Q to activate your heal ability.");
  set_line_width 2;
  set_color red;
  draw_player player1;
  draw_hp_bar ();
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  player_projectiles := move_projectiles !player_projectiles;
  draw_enemies ();
  update_enemies ();
  draw_projectiles !enemy_projectiles;
  enemy_projectiles := move_projectiles !enemy_projectiles;
  synchronize ()

let draw_tutorial_room_3 () = ()

(* draw_projectiles !enemy_projectiles; draw_enemy enemy; update_enemy enemy;
   enemy_projectiles := move_projectiles !enemy_projectiles; *)

let rec clear_input_queue () =
  if key_pressed () then
    let _ = read_key () in
    clear_input_queue ()

let draw_game_over () =
  clear_all_projectiles ();
  change_hp player1 (-(get_hp player1 - 100));
  last_heal_time := 15.0;
  move_player_absolute player1 150 450;
  room_completed.completed <- false;
  room_counter.room_counter <- 0;
  clear_input_queue ();
  set_color black;
  fill_rect 0 0 1908 987;
  moveto 953 493;
  set_color red;
  draw_string "GAME OVER";
  draw_button "RESTART" "Start Menu";
  synchronize ()

let draw_screens keyword =
  match keyword with
  | "Start Menu" -> draw_start_menu ()
  | "Game Over" -> draw_game_over ()
  | "Tutorial Room 1" -> draw_tutorial_room_1 ()
  | "Tutorial Room 2" -> draw_tutorial_room_2 ()
  | "Tutorial Room 3" -> draw_tutorial_room_3 ()
  | "Room 1" -> ()
  | _ -> ()

let () =
  open_graph "";
  set_window_title "Amazing Game";
  while true do
    auto_synchronize false;
    open_graph " 1908x987";
    draw_screens keyword.word
  done
