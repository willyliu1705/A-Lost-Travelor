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

type new_room = { mutable new_room : bool }

let new_room = { new_room = false }

type list_of_enemies = {
  mutable list_of_enemies : Dungeon_crawler.Enemy.t list;
}

let list_of_enemies = { list_of_enemies = [] }
let brown = rgb 150 75 0
let gray = rgb 211 211 211

(* player has to be sufficiently small or else weird interactions will occur
   with the enemy entities (e.g. enemy sees the player "faster" when entering
   the enemy's line of sight from the right compared to the left; as a result,
   this causes the enemy to not shoot at the player when its supposed to). *)
let player1 = create_player 150 450 30 30

let walls =
  [
    (* Top wall *)
    create_wall 0 60 1810 60;
    (* Bottom wall *)
    create_wall 0 890 1810 890;
    (* Left wall *)
    create_wall 41 60 60 890;
    (* Right wall *)
    create_wall 1810 60 1810 890;
  ]

let changed_walls =
  [
    create_wall 0 60 910 60;
    create_wall 1015 60 1810 60;
    create_wall 0 890 910 890;
    create_wall 1015 890 1810 890;
    create_wall 60 60 60 890;
    create_wall 1810 60 1810 400;
    create_wall 1810 540 1810 890;
  ]

let get_current_walls () =
  (* Return the changed walls if the room is completed *)
  if room_completed.completed then changed_walls
  else walls (* Return the original walls if the room is not completed *)

let draw_rect_centered x y w h = draw_rect (x - (w / 2)) (y - (h / 2)) w h
let fill_rect_centered x y w h = fill_rect (x - (w / 2)) (y - (h / 2)) w h

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

let update_player player walls =
  handle_enemy_projectiles_with_player enemy_projectiles player;
  if key_pressed () then
    match read_key () with
    | key -> (
        match of_key key with
        | Some dir ->
            player_direction := dir;
            let dx, dy = to_player_delta dir in
            let current_walls = get_current_walls () in
            move_player_no_collision player dx dy current_walls
        | None ->
            if key = ' ' then
              let () =
                player_shoot player player_projectiles !player_direction
              in
              change_hp player (-1)
            else if key = 'q' then
              let current_time = Unix.gettimeofday () in
              if current_time -. !last_heal_time >= 15.0 then (
                change_hp player 20;
                last_heal_time := current_time))

let handle_player_projectiles_with_enemies projectiles_ref enemies =
  enemies.list_of_enemies <-
    List.filter
      (fun enemy ->
        let initial_projectile_count = List.length !projectiles_ref in
        handle_projectile_collision_with_enemy projectiles_ref enemy;
        let remaining_projectile_count = List.length !projectiles_ref in
        initial_projectile_count = remaining_projectile_count)
      enemies.list_of_enemies

let draw_heal_ability () =
  set_line_width 3;
  set_color white;
  fill_rect 90 27 41 45;
  set_color black;
  draw_rect_centered 110 50 45 50;
  set_color gray;
  draw_rect_centered 110 50 32 8;
  draw_rect_centered 110 50 8 32;
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
    (Random.int 1607 + 100)
    (Random.int 686 + 100)
    (Random.int 10 + get_width player1)
    (Random.int 10 + get_height player1)
    array_of_possible_directions.(Random.int 4)
    (1. +. (float_of_int room_difficulty /. 10.))
    (let shooting_delay = 5. -. Random.float (float_of_int room_difficulty) in
     if shooting_delay >= 0.25 then shooting_delay else 0.25)

let draw_player player =
  let x = current_x_pos player in
  let y = current_y_pos player in
  let w = get_width player in
  let h = get_height player in
  set_color red;
  draw_rect x y w h;
  set_color black;
  draw_arc (x + w) (y + h) 5 5 (-90) 180;
  set_color red;
  if !player_direction = right then
    draw_poly_line
      [| (x + (3 * w / 4), y + (h / 4)); (x + (3 * w / 4), y + (3 * h / 4)) |]
  else if !player_direction = left then
    draw_poly_line
      [| (x + (w / 4), y + (h / 4)); (x + (w / 4), y + (3 * h / 4)) |]
  else if !player_direction = up then
    draw_poly_line
      [| (x + (w / 4), y + (3 * h / 4)); (x + (3 * w / 4), y + (3 * h / 4)) |]
  else if !player_direction = down then
    draw_poly_line
      [| (x + (w / 4), y + (h / 4)); (x + (3 * w / 4), y + (h / 4)) |]

(* Enemies should be the same size or larger than the player to prevent
   unintended interactions and misalignment issues for enemy line of sight
   between the enemy and the player (an example is described above). *)
let draw_enemy enemy =
  let x = enemy_x_pos enemy in
  let y = enemy_y_pos enemy in
  let w = get_enemy_width enemy in
  let h = get_enemy_height enemy in
  set_color blue;
  draw_rect x y w h;
  if get_direction enemy = left then
    draw_poly_line
      [| (x + (w / 4), y + (h / 4)); (x + (w / 4), y + (h * 3 / 4)) |]
  else if get_direction enemy = right then
    draw_poly_line
      [| (x + (w * 3 / 4), y + (h / 4)); (x + (w * 3 / 4), y + (h * 3 / 4)) |]
  else if get_direction enemy = down then
    draw_poly_line
      [| (x + (w / 4), y + (h / 4)); (x + (w * 3 / 4), y + (h / 4)) |]
  else if get_direction enemy = up then
    draw_poly_line
      [| (x + (w / 4), y + (h * 3 / 4)); (x + (w * 3 / 4), y + (h * 3 / 4)) |];

  set_color black

let draw_enemies enemies = List.iter draw_enemy enemies.list_of_enemies

let update_enemies_and_projectiles projectiles_ref enemies =
  handle_player_projectiles_with_enemies projectiles_ref enemies;
  let current_time = Unix.gettimeofday () in
  List.iter
    (fun enemy ->
      if aligned_with_player enemy (current_x_pos player1, current_y_pos player1)
      then
        enemy_shoot enemy enemy_projectiles enemy_last_shot_time
          (get_shooting_delay enemy) current_time)
    enemies.list_of_enemies;
  player_projectiles := move_projectiles !player_projectiles

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
  set_line_width 2;
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

let check_enter_next_room next_room =
  if room_completed.completed then
    if
      current_x_pos player1 >= 903
      && current_x_pos player1 <= 1004
      && current_y_pos player1 <= 0
    then (
      let () = keyword.word <- next_room in
      move_player_absolute player1 950 850;
      new_room.new_room <- true;
      room_counter.room_counter <- room_counter.room_counter + 1)
    else if
      current_x_pos player1 >= 903
      && current_x_pos player1 <= 1004
      && current_y_pos player1 >= 987
    then (
      let () = keyword.word <- next_room in
      move_player_absolute player1 950 150;
      new_room.new_room <- true;
      room_counter.room_counter <- room_counter.room_counter + 1)
    else if
      current_x_pos player1 >= 1908
      && current_y_pos player1 >= 443
      && current_y_pos player1 <= 543
    then (
      let () = keyword.word <- next_room in
      move_player_absolute player1 150 493;
      new_room.new_room <- true;
      room_counter.room_counter <- room_counter.room_counter + 1)

let check_room_completion () =
  if list_of_enemies.list_of_enemies = [] then room_completed.completed <- true
  else room_completed.completed <- false

let create_enemies_for_new_room () =
  if new_room.new_room = true then
    for x = 1 to 3 + room_counter.room_counter do
      list_of_enemies.list_of_enemies <-
        create_random_enemy room_counter.room_counter
        :: list_of_enemies.list_of_enemies
    done;
  new_room.new_room <- false

let draw_room_number () =
  set_color black;
  moveto 1800 950;
  draw_string ("Room Number: " ^ string_of_int room_counter.room_counter);
  draw_rect 1795 945 100 18

let draw_tutorial_room_1 () =
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () =
       draw_string "Great! Now walk through any of the doors that opened."
     in
     check_enter_next_room "Tutorial Room 2"
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
  draw_room_number ();
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  player_projectiles := move_projectiles !player_projectiles;
  draw_pressure_plate 954 493 50 50;
  synchronize ()

let draw_tutorial_room_2 () =
  create_enemies_for_new_room ();
  check_room_completion ();
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () =
       draw_string "Nice job! Complete 1 more room to finish the tutorial."
     in
     check_enter_next_room "Tutorial Room 3"
   else
     let () = draw_closed_room_doors () in
     let () = set_color brown in
     let () = moveto 30 920 in
     let () =
       draw_string
         "You can press SPACEBAR to shoot projectiles at the blue enemies. \
          Every time you fire a projectile though, you will lose HP. "
     in
     let () = moveto 30 900 in
     draw_string
       "Fire sparingly! You can also press Q to activate your heal ability.");
  set_line_width 2;
  set_color red;
  draw_player player1;
  draw_hp_bar ();
  draw_room_number ();
  draw_enemies list_of_enemies;
  update_enemies_and_projectiles player_projectiles list_of_enemies;
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  draw_projectiles !enemy_projectiles;
  enemy_projectiles := move_projectiles !enemy_projectiles;
  synchronize ()

let draw_tutorial_room_3 () =
  create_enemies_for_new_room ();
  check_room_completion ();
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () = draw_string "Best of luck! The real game begins now." in
     check_enter_next_room "Room 4"
   else
     let () = draw_closed_room_doors () in
     let () = set_color brown in
     let () = moveto 30 920 in
     let () =
       draw_string
         "As you progress through each room, there will be more enemies and \
          they will become stronger."
     in
     let () = moveto 30 900 in
     draw_string
       "Fire sparingly! You can also press Q to activate your heal ability.");
  set_line_width 2;
  set_color red;
  draw_player player1;
  draw_hp_bar ();
  draw_room_number ();
  draw_enemies list_of_enemies;
  update_enemies_and_projectiles player_projectiles list_of_enemies;
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  draw_projectiles !enemy_projectiles;
  enemy_projectiles := move_projectiles !enemy_projectiles;
  synchronize ()

let draw_room_4 () =
  create_enemies_for_new_room ();
  check_room_completion ();
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () = draw_string "You sure you want to keep going?" in
     check_enter_next_room "Room 5"
   else
     let () = draw_closed_room_doors () in
     let () = set_color brown in
     let () = moveto 30 920 in
     draw_string "Are you tired yet?");
  set_line_width 2;
  set_color red;
  draw_player player1;
  draw_hp_bar ();
  draw_room_number ();
  draw_enemies list_of_enemies;
  update_enemies_and_projectiles player_projectiles list_of_enemies;
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  draw_projectiles !enemy_projectiles;
  enemy_projectiles := move_projectiles !enemy_projectiles;
  synchronize ()

let draw_room_5 () =
  create_enemies_for_new_room ();
  check_room_completion ();
  draw_normal_room_boundaries ();
  (if room_completed.completed then
     let () = draw_open_room_doors () in
     let () = set_color brown in
     let () = moveto 30 900 in
     let () = draw_string "You sure you want to keep going?" in
     check_enter_next_room "Room 4"
   else
     let () = draw_closed_room_doors () in
     let () = set_color brown in
     let () = moveto 30 920 in
     draw_string "Keep going!");
  set_line_width 2;
  set_color red;
  draw_player player1;
  draw_hp_bar ();
  draw_room_number ();
  draw_enemies list_of_enemies;
  update_enemies_and_projectiles player_projectiles list_of_enemies;
  draw_heal_ability ();
  update_player player1 walls;
  draw_projectiles !player_projectiles;
  draw_projectiles !enemy_projectiles;
  enemy_projectiles := move_projectiles !enemy_projectiles;
  synchronize ()

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
  list_of_enemies.list_of_enemies <- [];
  clear_input_queue ();
  set_color black;
  fill_rect 0 0 1908 987;
  moveto 925 493;
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
  | "Room 4" -> draw_room_4 ()
  | "Room 5" -> draw_room_5 ()
  | _ -> ()

let () =
  open_graph "";
  set_window_title "A Lost Traveler";
  while true do
    auto_synchronize false;
    open_graph " 1908x987";
    draw_screens keyword.word
  done
