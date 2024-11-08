open Graphics
open Dungeon_crawler.Player

let player1 = create_player 40 40 50 50

let draw_player player =
  draw_rect (current_x_pos player) (current_y_pos player) (get_height player)
    (get_width player)

let update_player player =
  match read_key () with
  | 'w' -> move_player player 0 10
  | 's' -> move_player player 0 (-10)
  | 'a' -> move_player player (-10) 0
  | 'd' -> move_player player 10 0
  | _ -> ()

let check_press_start player =
  let mouse_position = mouse_pos () in
  if button_down () then
    match mouse_position with
    | x, y ->
        if x >= 766 && x <= 1141 && y >= 133 && y <= 253 then
          move_player player1 10 10

(** [draw_rect_centered] draws the rectangle centered at point [x], [y] with
    width [w] and height [h].*)
let draw_rect_centered x y w h = draw_rect (x - (w / 2)) (y - (h / 2)) w h

let () =
  while true do
    auto_synchronize false;
    open_graph " 1908x987";
    set_window_title "Game";
    set_color black;
    draw_rect 0 0 1907 986;
    fill_rect 0 0 1907 986;
    set_color white;
    (* draw_poly_line [| (954, 986); (954, 0) |]; *)
    draw_poly_line [| (1354, 986); (1354, 0) |];
    draw_poly_line [| (554, 986); (554, 0) |];
    draw_circle 954 483 10;
    draw_rect_centered 954 193 375 120;
    moveto 913 193;
    draw_string "PRESS TO START";
    draw_arc 954 800 400 100 0 180;
    draw_arc 954 200 400 100 180 360;
    draw_player player1;
    update_player player1;
    synchronize ()
  done

(* let get_corners player = {}

   let collision_check (obj1 : Dungeon_crawler.Player.t) (obj2:
   Dungeon_crawler.Player.t) dx dy = let corners_obj1 = get_corners obj1 in let
   corners_obj2 = get_corners obj2 in if corners_obj1 *)
