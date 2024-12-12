type t =
  | Up
  | Down
  | Left
  | Right

let up = Up
let down = Down
let left = Left
let right = Right

let is_up = function
  | Up -> true
  | Down -> false
  | Left -> false
  | Right -> false

let is_down = function
  | Up -> false
  | Down -> true
  | Left -> false
  | Right -> false

let is_left = function
  | Up -> false
  | Down -> false
  | Left -> true
  | Right -> false

let is_right = function
  | Up -> false
  | Down -> false
  | Left -> false
  | Right -> true

let player_speed = 10
let player_projectile_speed = 5
let player_direction = ref right

let to_player_delta = function
  | Up -> (0, player_speed)
  | Down -> (0, -player_speed)
  | Left -> (-player_speed, 0)
  | Right -> (player_speed, 0)

let to_player_projectile_delta = function
  | Up -> (0, player_projectile_speed)
  | Down -> (0, -player_projectile_speed)
  | Left -> (-player_projectile_speed, 0)
  | Right -> (player_projectile_speed, 0)

let of_key = function
  | 'w' -> Some Up
  | 's' -> Some Down
  | 'a' -> Some Left
  | 'd' -> Some Right
  | _ -> None

let direction_to_string = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
