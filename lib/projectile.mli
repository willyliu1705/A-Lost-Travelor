type t
(** Abstract type representing a projectile. *)

val create_projectile : int -> int -> int -> int -> t
(** [create_projectile x y dx dy] creates a projectile at position ([x], [y])
    with a velocity ([dx], [dy]). *)

val move_all : t list -> t list
(** [move_all projectiles] moves all projectiles in the list [projectiles]
    according to their velocity and removes those out of bounds. *)

val draw_all : t list -> unit
(** [draw_all projectiles] draws all projectiles in the list [projectiles]. *)
