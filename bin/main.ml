open Graphics
open Dungeon_crawler.Player

type direction =
  | Up
  | Down
  | Left
  | Right

type projectile = {
  x : int;
  y : int;
  dx : int;
  dy : int;
}

let player1 = create_player 40 40 50 50
let projectiles = ref []

let player_direction =
  ref Right (* Change default direction here if necessary. *)

let draw_player player =
  draw_rect (current_x_pos player) (current_y_pos player) (get_height player)
    (get_width player)

let draw_projectiles () =
  List.iter (fun p -> draw_rect p.x p.y 5 5) !projectiles

let move_projectiles () =
  projectiles :=
    List.map (fun p -> { p with x = p.x + p.dx; y = p.y + p.dy }) !projectiles;
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

(* let get_corners player = {}

   let collision_check (obj1 : Dungeon_crawler.Player.t) (obj2:
   Dungeon_crawler.Player.t) dx dy = let corners_obj1 = get_corners obj1 in let
   corners_obj2 = get_corners obj2 in if corners_obj1 *)

let () =
  open_graph "";
  while true do
    auto_synchronize false;
    clear_graph ();
    draw_rect 4 5 300 200;
    draw_player player1;
    draw_projectiles ();
    update_player player1;
    move_projectiles ();
    synchronize ()
  done
