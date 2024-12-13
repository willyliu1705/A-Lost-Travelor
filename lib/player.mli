type t
(** Abstract type representing the player. *)

val create_player : int -> int -> int -> int -> t
(** [create_player x y w h] creates a player at position ([x], [y]) in Cartesian
    coordinates with width [w] and height [h]. The player is always initialized
    with an hp value of 100. Requires: [w] > 0 and [h] > 0. *)

val move_player : t -> int -> int -> unit
(** [move_player player dx dy] moves the [player] by ([dx], [dy]) to a new
    coordinate (x + [dx], y + [dy]). *)

val move_player_absolute : t -> int -> int -> unit
(** [move_player_absolute] moves [player] to the coordinates [x], [y]. *)

val current_x_pos : t -> int
(** [current_x_pos player] is the current x-position of [player] as a coordinate
    on the xy-plane. *)

val current_y_pos : t -> int
(** [current_x_pos player] is the current y-position of [player] as a coordinate
    on the xy-plane. *)

val get_height : t -> int
(** [get_height player] is the height of [player]. *)

val get_width : t -> int
(** [get_width player] is the width of [player]. *)

val get_hp : t -> int
(** [get_hp player] is the current hp of [player]. *)

val change_hp : t -> int -> unit
(** [change_hp player amount] adjusts the hp value of [player] according to the
    provided [amount]. *)

val last_heal_time : float ref
(** [last_heal_time] is the timestamp of the player's last heal action. *)

val player_shoot : t -> Projectile.t list ref -> Direction.t -> unit
(** [player_shoot player projectiles_ref direction] adds a new projectile to
    [projectiles_ref], fired by [player] in the given [direction]. The
    projectile is created at the center of the player's current position and
    moves in the direction specified. *)

val handle_enemy_projectiles_with_player : Projectile.t list ref -> t -> unit
(** [handle_enemy_projectiles_with_player enemy_projectiles player] removes all
    projectiles in [enemy_projectiles] that collide with the given [player] and
    decreases the player's HP by 10 for each collision. *)
