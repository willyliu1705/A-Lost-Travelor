type t
(** Abstract type representing a direction. *)

val up : t
(** [up] is the upward direction. *)

val down : t
(** [down] is the downward direction. *)

val left : t
(** [left] is the leftward direction. *)

val right : t
(** [right] is the rightward direction. *)

val is_up : t -> bool
(** [is_up direction] is true if [direction] is up and false otherwise. *)

val is_down : t -> bool
(** [is_down direction] is true if [direction] is down and false otherwise. *)

val is_left : t -> bool
(** [is_left direction] is true if [direction] is left and false otherwise. *)

val is_right : t -> bool
(** [is_right direction] is true if [direction] is right and false otherwise. *)

val player_speed : int
(** [player_speed] is the default speed of the player. *)

val player_projectile_speed : int
(** [player_projectile_speed] is the default speed of a projectile fired by the
    player. *)

val player_direction : t ref
(** [player_direction] is the default direction of the player. *)

val to_player_delta : t -> int * int
(** [to_player_delta direction] converts a direction to a tuple representing the
    change in coordinates (dx, dy). *)

val to_player_projectile_delta : t -> int * int
(** [to_player_projectile_delta direction] converts a direction to a tuple
    representing the change in coordinates (dx, dy) for projectiles. *)

val of_key : char -> t option
(** [of_key key] maps a key press to a direction and is [Some direction] for
    valid keys and [None] otherwise. *)

val direction_to_string : t -> string
(** [direction_to_string direction] is the string representation of [direction]. *)
