open Graphics
open Dungeon_crawler.Player

type projectile = {
  x : int;
  y : int;
  speed : int;
}

let player1 = create_player 40 40 50 50
let projectiles = ref []

let draw_player player =
  draw_rect (current_x_pos player) (current_y_pos player) (get_height player)
    (get_width player)

let draw_projectiles () =
  List.iter (fun p -> draw_rect p.x p.y 5 5) !projectiles

let move_projectiles () =
  projectiles := List.map (fun p -> { p with x = p.x + p.speed }) !projectiles;
  projectiles := List.filter (fun p -> p.x < size_x ()) !projectiles

let shoot player =
  let new_projectile =
    {
      x = current_x_pos player + (get_width player / 2);
      y = current_y_pos player + (get_height player / 2);
      speed = 1;
    }
  in
  projectiles := new_projectile :: !projectiles

let update_player player =
  if key_pressed () then
    match read_key () with
    | 'w' -> move_player player 0 2
    | 's' -> move_player player 0 (-2)
    | 'a' -> move_player player (-2) 0
    | 'd' -> move_player player 2 0
    | ' ' -> shoot player (* ' ' indicates spacebar *)
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
