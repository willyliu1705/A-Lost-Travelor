open Player

(* Defining types for projectiles and walls *)
type projectile = {
  x : int;
  y : int;
  dx : int;
  dy : int;
}

type wall = {
  x : int;
  y : int;
  width : int;
  height : int;
}

(* Function to check if two rectangles (x, y, width, height) intersect *)
let rectangles_intersect (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

(* Check if the player collides with any projectiles *)
let check_player_projectile_collision (player : Player.t)
    (projectiles : projectile list) : bool =
  (* Get the player's position and dimensions from the Player module *)
  let player_rect =
    ( Player.current_x_pos player,
      Player.current_y_pos player,
      Player.get_width player,
      Player.get_height player )
  in
  (* Check if the player's rectangle intersects with any projectile's
     rectangle *)
  List.exists
    (fun (p : projectile) ->
      let proj_rect = (p.x, p.y, 5, 5) in
      rectangles_intersect player_rect proj_rect)
    projectiles

(* Check if the player collides with any walls *)
let check_player_wall_collision (player : Player.t) (walls : wall list) : bool =
  (* Get the player's position and dimensions from the Player module *)
  let player_rect =
    ( Player.current_x_pos player,
      Player.current_y_pos player,
      Player.get_width player,
      Player.get_height player )
  in
  (* Check if the player's rectangle intersects with any wall's rectangle *)
  List.exists
    (fun (w : wall) ->
      let wall_rect = (w.x, w.y, w.width, w.height) in
      rectangles_intersect player_rect wall_rect)
    walls
