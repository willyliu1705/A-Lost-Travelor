(* Type definition for Wall *)
type t

val create_wall : int -> int -> int -> int -> t
(** [create_wall x y width height] creates a wall at position ([x], [y]) with
    dimensions ([width], [height]). *)

val get_wall_position : t -> int * int
(** [get_wall_position wall] is the (x, y) position of [wall]. *)

val get_wall_size : t -> int * int
(** [get_wall_size wall] is the (width, height) size of [wall]. *)

val in_wall_bounds : t -> int -> int -> bool
(** [in_bounds wall width height] checks if [wall] is within the bounds of the
    screen defined by [width] and [height]. *)
