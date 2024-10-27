type t = {
  mutable x : int;
  mutable y : int;
      (* store additional information about player in future sprints (e.g.
         inventory) *)
}

let create_player x y = { x; y }

let move_player player dx dy =
  player.x <- player.x + dx;
  player.y <- player.y + dy

let current_position player = (player.x, player.y)
