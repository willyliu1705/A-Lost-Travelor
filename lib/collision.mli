open Player

type projectile = {
  x : int;
  y : int;
  dx : int;
  dy : int;
}

(* Type for walls *)
type wall = {
  x : int;
  y : int;
  width : int;
  height : int;
}

(* Function to check if two rectangles (represented by their x, y, width,
   height) intersect *)
val rectangles_intersect :
  int * int * int * int -> int * int * int * int -> bool

(* Check if the player collides with any projectiles *)
val check_player_projectile_collision : Player.t -> projectile list -> bool

(* Check if the player collides with any walls *)
val check_player_wall_collision : Player.t -> wall list -> bool
