type t
(** Abstract type representing a projectile. *)

val create_proj : int -> int -> int -> int -> t
(** [create_proj x y dx dy] creates a projectile at position ([x], [y]) with a
    velocity ([dx], [dy]). *)

val move_proj : t -> t
(** [move_proj projectile] returns a new projectile with updated position based
    on its velocity. *)

val in_bounds : t -> int -> int -> bool
(** [in_bounds projectile width height] checks if [projectile] is within bounds
    of the screen defined by [width] and [height]. *)

val get_proj_position : t -> int * int
(** [get_proj_position projectile] is the (x, y) position of [projectile]. *)
