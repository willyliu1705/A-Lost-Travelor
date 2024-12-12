type t
(** Abstract type representing a projectile. *)

val player_projectiles : t list ref
(** [player_projectiles] is the list of projectiles the player has fired. *)

val enemy_projectiles : t list ref
(** [enemy_projectiles] is the list of projectiles an enemy has fired. *)

val create_proj : int -> int -> int -> int -> t
(** [create_proj x y dx dy] creates a projectile at position ([x], [y]) with a
    velocity ([dx], [dy]). *)

val move_proj : t -> t
(** [move_proj projectile] is a new projectile with updated position based on
    its velocity. *)

val in_bounds : t -> int -> int -> bool
(** [in_bounds projectile width height] checks if [projectile] is within bounds
    of the screen defined by [width] and [height]. *)

val get_proj_position : t -> int * int
(** [get_proj_position projectile] is the (x, y) position of [projectile]. *)

val handle_collision : t list ref -> int -> int -> int -> int -> unit
(** [handle_collision projectiles_ref x y w h] removes all projectiles in
    [projectiles_ref] that collide with the rectangular object at ([x], [y])
    with dimensions [w] and [h]. *)

val detect_collision : t list ref -> int -> int -> int -> int -> bool
(** [detect_collision projectiles_ref x y w h] is true if any projectile in
    [projectiles_ref] collides with a rectangular object at ([x], [y]) with
    dimensions [w] and [h] and false otherwise. *)
