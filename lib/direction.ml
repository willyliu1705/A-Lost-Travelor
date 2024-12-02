type t =
  | Up
  | Down
  | Left
  | Right

let up = Up
let down = Down
let left = Left
let right = Right

let to_delta = function
  | Up -> (0, 5)
  | Down -> (0, -5)
  | Left -> (-5, 0)
  | Right -> (5, 0)

let of_key = function
  | 'w' -> Some Up
  | 's' -> Some Down
  | 'a' -> Some Left
  | 'd' -> Some Right
  | _ -> None
