type t
(** Abstract type representing a direction. *)

val up : t
(** The direction Up. *)

val down : t
(** The direction Down. *)

val left : t
(** The direction Left. *)

val right : t
(** The direction Right. *)

val to_delta : t -> int * int
(** [to_delta direction] converts a direction to a tuple representing the change
    in coordinates (dx, dy). *)

val of_key : char -> t option
(** [of_key key] maps a key press to a direction. Returns [Some direction] for
    valid keys and [None] otherwise. *)
