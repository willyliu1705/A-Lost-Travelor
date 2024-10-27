type t
(** Abstract type representing the player. *)

val create_player : int -> int -> t
(** [create_player x y] creates a player at position (x, y) in Cartesian
    coordinates. *)

val move_player : t -> int -> int -> unit
(** [move_player player dx dy] moves the [player] by ([dx], [dy]) to a new
    coordinate (x + [dx], y + [dy]). *)

val current_position : t -> int * int
(** [current_position player] is the current position of the player as a
    coordinate on the xy-plane. *)
