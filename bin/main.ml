open Graphics
open Dungeon_crawler.Player

let player1 = create_player 40 40 50 50

let draw_player player =
  draw_rect (current_x_pos player) (current_y_pos player) (get_height player)
    (get_width player)

let update_player player =
  match read_key () with
  | 'w' -> move_player player 0 2
  | 's' -> move_player player 0 (-2)
  | 'a' -> move_player player (-2) 0
  | 'd' -> move_player player 2 0
  | _ -> ()

(* let get_corners player = {}

   let collision_check (obj1 : Dungeon_crawler.Player.t) (obj2:
   Dungeon_crawler.Player.t) dx dy = let corners_obj1 = get_corners obj1 in let
   corners_obj2 = get_corners obj2 in if corners_obj1 *)

let () =
  while true do
    auto_synchronize false;
    open_graph "";
    draw_rect 4 5 300 200;
    draw_player player1;
    update_player player1;
    synchronize ()
  done
