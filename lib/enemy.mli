type t
(** Abstract type representing an enemy. *)

val enemy_last_shot_time : float ref
(** [enemy_last_shot_time] is the time of the last shot by an enemy. *)

val create_enemy : int -> int -> int -> int -> Direction.t -> int -> float -> t
(** [create_enemy x y w h dir projectile_speed shooting_delay] creates an enemy
    at position ([x], [y]) in Cartesian coordinates with width [w], height [h],
    facing direction [dir], [projectile_speed], and [shooting_delay]. Requires:
    [w] > 0, [h] > 0, and [projectile_speed] > 0. *)

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

val get_projectile_speed : t -> int
(** [get_projectile_speed enemy] is the projectile speed of [enemy]. *)

val get_shooting_delay : t -> float
(** [get_shooting_delay enemy] is the shooting delay of [enemy]. *)

val enemy_shoot :
  t -> Projectile.t list ref -> float ref -> float -> float -> unit
(** [enemy_shoot enemy projectiles_ref last_shot_time delay current_time] shoots
    a projectile from [enemy] if enough time has passed since the last shot,
    updating [last_shot_time]. *)

val aligned_with_player : t -> int * int -> bool
(** [aligned_with_player enemy (px, py)] checks if the player's coordinates
    [(px, py)] is aligned with [enemy] based on [enemy]'s direction. In other
    words, if either of the player's x or y-coordinates are close in value to
    the enemy's x or y-coordinates and the enemy is also facing in that specific
    direction, then the enemy can "see" the player and any enemy actions will
    subsequently begin. Once the player leaves that "line of sight", then enemy
    action ceases. *)
