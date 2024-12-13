type t
(* Abstract type representing a wall. *)

val create_wall : int -> int -> int -> int -> t
(** [create_wall x y width height] creates a wall at position ([x], [y]) with
    [width] and [height]. Requires: [width] > 0 and [height] > 0.*)

val wall_x_pos : t -> int
(** [wall_x_pos wall] is the current x-coordinate of [wall] on the xy-plane. *)

val wall_y_pos : t -> int
(** [wall_y_pos wall] is the current y-coordinate of [wall] on the xy-plane. *)

val get_wall_height : t -> int
(** [get_wall_height wall] is the height of [wall]. *)

val get_wall_width : t -> int
(** [get_wall_height wall] is the width of [wall]. *)

val in_wall_bounds : t -> int -> int -> bool
(** [in_wall_bounds wall width height] checks if [wall] is within the bounds of
    the screen defined by [width] and [height]. *)
