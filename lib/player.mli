type t
(** Abstract type representing the player. *)

val create_player : int -> int -> int -> int -> t
(** [create_player x y] creates a player at position (x, y) in Cartesian
    coordinates with a specified length and width. *)

val move_player : t -> int -> int -> unit
(** [move_player player dx dy] moves the [player] by ([dx], [dy]) to a new
    coordinate (x + [dx], y + [dy]). *)

val current_x_pos : t -> int
(** [current_x_pos character] is the current x-position of the player as a
    coordinate on the xy-plane. *)

val current_y_pos : t -> int
(** [current_x_pos character] is the current y-position of the player as a
    coordinate on the xy-plane. *)

val get_height : t -> int
val get_width : t -> int
val get_corners : t -> t
