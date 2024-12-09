type t =
  | Up
  | Down
  | Left
  | Right

let up = Up
let down = Down
let left = Left
let right = Right
let player_speed = 10
let projectile_speed = 5

let to_player_delta = function
  | Up -> (0, player_speed)
  | Down -> (0, -player_speed)
  | Left -> (-player_speed, 0)
  | Right -> (player_speed, 0)

let to_projectile_delta = function
  | Up -> (0, projectile_speed)
  | Down -> (0, -projectile_speed)
  | Left -> (-projectile_speed, 0)
  | Right -> (projectile_speed, 0)

let of_key = function
  | 'w' -> Some Up
  | 's' -> Some Down
  | 'a' -> Some Left
  | 'd' -> Some Right
  | _ -> None
