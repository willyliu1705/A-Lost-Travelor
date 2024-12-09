type t
(** Abstract type representing an enemy. *)

val create_enemy : int -> int -> int -> int -> Direction.t -> t
(** [create_enemy x y w h dir] creates an enemy at position ([x], [y]) in
    Cartesian coordinates with width [w], height [h], and direction [dir].
    Requires: [w] > 0 and [h] > 0. *)

val enemy_x_pos : t -> int
(** [enemy_x_pos enemy] is the current x-coordinate of [enemy] on the xy-plane. *)

val enemy_y_pos : t -> int
(** [enemy_x_pos enemy] is the current y-coordinate of [enemy] on the xy-plane . *)

val get_enemy_height : t -> int
(** [get_enemy_height enemy] is the height of [enemy]. *)

val get_enemy_width : t -> int
(** [get_enemy_height enemy] is the width of [enemy]. *)

val set_direction : t -> Direction.t -> unit
(** [set_direction enemy dir] sets the direction of [enemy] to [dir]. *)

val get_direction : t -> Direction.t
(** [get_direction enemy] is the current direction of [enemy]. *)

val enemy_shoot :
  t -> Projectile.t list ref -> float ref -> float -> float -> unit
(** [enemy_shoot enemy projectiles_ref last_shot_time delay current_time] shoots
    a projectile from [enemy] if enough time has passed since the last shot,
    updating [last_shot_time]. *)

val aligned_with_player : t -> int * int -> bool
(** [aligned_with_player enemy (px, py)] checks if the player is aligned with
    [enemy] based on [enemy]'s direction. *)
