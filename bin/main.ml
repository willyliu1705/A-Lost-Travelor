open Graphics
open Dungeon_crawler.Player
open Dungeon_crawler.Collision

type direction =
  | Up
  | Down
  | Left
  | Right

let projectiles : projectile list ref = ref []

(* Reference to the list of walls *)
let walls : wall list ref =
  ref
    [
      { x = 100; y = 200; width = 50; height = 100 };
      { x = 300; y = 400; width = 75; height = 200 };
    ]

type keyword = { mutable word : string }

let keyword = { word = "Start Menu" }
let player1 = create_player 40 40 50 50

let player_direction =
  ref Right (* Change default direction here if necessary. *)

let draw_player player =
  draw_rect (current_x_pos player) (current_y_pos player) (get_height player)
    (get_width player)

let draw_projectiles () =
  List.iter (fun p -> draw_rect p.x p.y 5 5) !projectiles

let move_projectiles () =
  (* Update projectile positions by adding dx and dy to x and y *)
  projectiles :=
    List.map (fun p -> { p with x = p.x + p.dx; y = p.y + p.dy }) !projectiles;

  (* Filter out projectiles that are out of bounds *)
  projectiles :=
    List.filter
      (fun p -> p.x < size_x () && p.x >= 0 && p.y < size_y () && p.y >= 0)
      !projectiles

let shoot player =
  let dx, dy =
    match !player_direction with
    | Up -> (0, 5)
    | Down -> (0, -5)
    | Left -> (-5, 0)
    | Right -> (5, 0)
  in
  let new_projectile =
    {
      x = current_x_pos player + (get_width player / 2);
      y = current_y_pos player + (get_height player / 2);
      dx;
      dy;
    }
  in
  projectiles := new_projectile :: !projectiles

let update_player player =
  if key_pressed () then
    match read_key () with
    | 'w' ->
        move_player player 0 2;
        player_direction := Up
    | 's' ->
        move_player player 0 (-2);
        player_direction := Down
    | 'a' ->
        move_player player (-2) 0;
        player_direction := Left
    | 'd' ->
        move_player player 2 0;
        player_direction := Right
    | ' ' -> shoot player
    | _ -> ()

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
      update_player player1;
      draw_projectiles ();
      move_projectiles ();
      (* Check for collisions with projectiles *)
      let projectile_collision =
        check_player_projectile_collision player1 !projectiles
      in
      if projectile_collision then
        (* Handle collision with projectiles (e.g., reduce player health,
           etc.) *)
        print_endline "Player hit by projectile!";

      (* Check for collisions with walls *)
      let wall_collision = check_player_wall_collision player1 !walls in
      if wall_collision then print_endline "Player hit a wall!"
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
