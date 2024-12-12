open Graphics
open Dungeon_crawler.Player
open Dungeon_crawler.Projectile
open Dungeon_crawler.Direction
open Dungeon_crawler.Enemy

type keyword = { mutable word : string }

let keyword = { word = "Start Menu" }

type room_completed = { mutable completed : bool }

let room_completed = { completed = false }
let brown = rgb 150 75 0
let player1 = create_player 150 450 30 30
(* player has to be sufficiently small or else weird interactions will occur
   with the enemy entities (e.g. enemy sees the player "faster" when entering
   the enemy's line of sight from the right compared to the left; as a result,
   this causes the enemy to not shoot at the player when its supposed to. ) *)

let player_projectiles = ref []
let enemy_projectiles = ref []
let player_direction = ref right (* Change player default direction here *)

(* Enemies should be the same size or larger than the player to prevent
   unintended interactions and misalignment issues for enemy line of sight
   between the enemy and the player (an example is described above). *)
let enemy1 = create_enemy 400 500 50 50 up 1 1.0
let enemy2 = create_enemy 200 250 10 10 right 1 2.0
let enemy3 = create_enemy 350 700 10 10 right 1 0.5

(** [draw_rect_centered] draws the rectangle centered at point [x], [y] with
    width [w] and height [h].*)
let draw_rect_centered x y w h = draw_rect (x - (w / 2)) (y - (h / 2)) w h

let draw_player player =
  let () =
    draw_rect_centered (current_x_pos player) (current_y_pos player)
      (get_width player) (get_height player)
  in
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
              let () =
                player_shoot player player_projectiles !player_direction
              in
              change_hp player (-1))

let draw_enemies () =
  draw_enemy enemy1;
  draw_enemy enemy2;
  draw_enemy enemy3

let update_enemies () =
  update_enemy enemy1;
  update_enemy enemy2;
  update_enemy enemy3

(* let draw_pause_button () = set_color black; set_line_width 3; draw_rect 1800
   930 50 50; let mouse_position = mouse_pos () in if button_down () then match
   mouse_position with | x, y -> if x >= 1800 && x <= 1850 && y >= 930 && y <=
   980 then paused.paused <- true; synchronize () *)

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
  (* Add some stars as background *)
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

(* let draw_start_room_boundaries () = set_color black; set_line_width 10;
   draw_segments [| (0, 886, 903, 886); (1004, 886, 1807, 886); (1807, 886,
   1807, 543); (1807, 443, 1807, 100); (1807, 100, 1004, 100); (5, 100, 5, 886);
   (903, 100, 0, 100); |] *)

(* THESE ARE THE COORDINATES OF THE WALLS *)
let draw_normal_room_boundaries () =
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
  update_player player1;
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
  update_player player1;
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

let draw_game_over () =
  change_hp player1 (-(get_hp player1 - 100));
  move_player_absolute player1 150 450;
  room_completed.completed <- false;
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
  | "Level 1" -> ()
  | _ -> ()

let () =
  open_graph "";
  set_window_title "Amazing Game";
  while true do
    auto_synchronize false;
    open_graph " 1908x987";
    draw_screens keyword.word
  done
