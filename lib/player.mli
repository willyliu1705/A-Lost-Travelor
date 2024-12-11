type t
(** Abstract type representing the player. *)

val create_player : int -> int -> int -> int -> t
(** [create_player x y w h] creates a player at position ([x], [y]) in Cartesian
    coordinates with width [w] and height [h]. The player is always initialized
    with an hp value of 100. Requires: [w] > 0 and [h] > 0. *)

val move_player : t -> int -> int -> unit
(** [move_player player dx dy] moves the [player] by ([dx], [dy]) to a new
    coordinate (x + [dx], y + [dy]). *)

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
(** [change_hp] adjusts the hp value of [player] according to the provided
    [d_hp]. *)

val get_corners : t -> t
(** [get_corners player] is a new player that has the same x,y-position as
    [player], but the height is increased by the value of the x-coprdinate and
    the width is increased by the value of the y-coordinate. *)
