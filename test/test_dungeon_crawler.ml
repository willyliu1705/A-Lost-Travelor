open OUnit2
open Dungeon_crawler.Player
open Dungeon_crawler.Projectile
open Dungeon_crawler.Direction

let string_of_tuple (x, y) = Printf.sprintf "(%d, %d)" x y

let string_of_option = function
  | Some _ -> "Some direction"
  | None -> "None"

(** [test_current_x name input expected_output] is a test case with [name] and
    checks if the current x of the player [input] is equal to [expected_output]. *)
let test_current_x name input expected_x =
  name >:: fun _ ->
  assert_equal expected_x (current_x_pos input) ~printer:string_of_int

(** [test_current_y name input expected_output] is a test case with [name] and
    checks if the current y of the player [input] is equal to [expected_output]. *)
let test_current_y name input expected_y =
  name >:: fun _ ->
  assert_equal expected_y (current_y_pos input) ~printer:string_of_int

(** [test_get_height name input expected_ouput] is a test case with [name] and
    checks if the current height of the player [input] is equal to
    [expected_output]. *)
let test_get_height name input expected_height =
  name >:: fun _ ->
  assert_equal expected_height (get_height input) ~printer:string_of_int

(** [test_get_width name input expected_ouput] is a test case with [name] and
    checks if the current width of the player [input] is equal to
    [expected_output]. *)
let test_get_width name input expected_width =
  name >:: fun _ ->
  assert_equal expected_width (get_width input) ~printer:string_of_int

(** [test_move_player name player dx dy expected_x expected_y] is a test case
    with [name] that checks if moving [player] by ([dx], [dy]) results in the
    expected position ([expected_x], [expected_y]). *)
let test_move_player name player dx dy expected_x expected_y =
  name >:: fun _ ->
  let () = move_player player dx dy in
  assert_equal expected_x (current_x_pos player) ~printer:string_of_int;
  assert_equal expected_y (current_y_pos player) ~printer:string_of_int

(** [test_get_corners name player expected_height expected_width] is a test case
    with [name] that checks if calling [get_corners] on [player] results in the
    correct new dimensions for height and width. *)
let test_get_corners name player expected_height expected_width =
  name >:: fun _ ->
  let corners = get_corners player in
  assert_equal expected_height (get_height corners) ~printer:string_of_int;
  assert_equal expected_width (get_width corners) ~printer:string_of_int

(** [test_create_proj name x y dx dy expected] is a test case with [name] that
    checks if creating a projectile with position ([x], [y]) and velocity ([dx],
    [dy]) results in the expected position and velocity. *)
let test_create_proj name x y dx dy expected_x expected_y expected_dx
    expected_dy =
  name >:: fun _ ->
  let proj = create_proj x y dx dy in
  assert_equal (expected_x, expected_y) (get_position proj)
    ~printer:(fun (x, y) -> Printf.sprintf "(%d, %d)" x y);
  assert_equal (expected_dx, expected_dy) (dx, dy) ~printer:string_of_tuple

(** [test_move_proj name proj expected_x expected_y] is a test case with [name]
    that checks if moving the projectile [proj] results in the expected position
    ([expected_x], [expected_y]). *)
let test_move_proj name proj expected_x expected_y =
  name >:: fun _ ->
  let moved_proj = move_proj proj in
  assert_equal (expected_x, expected_y) (get_position moved_proj)
    ~printer:string_of_tuple

(** [test_in_bounds name proj width height expected] is a test case with [name]
    that checks if the projectile [proj] is within the bounds defined by
    ([width], [height]). *)
let test_in_bounds name proj width height expected =
  name >:: fun _ ->
  assert_equal expected (in_bounds proj width height) ~printer:string_of_bool

(** [test_get_position name proj expected_x expected_y] is a test case with
    [name] that checks if the position of [proj] is ([expected_x],
    [expected_y]). *)
let test_get_position name proj expected_x expected_y =
  name >:: fun _ ->
  let actual_x, actual_y = get_position proj in
  assert_equal expected_x actual_x ~printer:string_of_int;
  assert_equal expected_y actual_y ~printer:string_of_int

(** [test_to_delta name dir expected_dx expected_dy] is a test case with [name]
    that checks if converting [dir] to a delta gives ([expected_dx],
    [expected_dy]). *)
let test_to_delta name dir expected_dx expected_dy =
  name >:: fun _ ->
  let actual_dx, actual_dy = to_delta dir in
  assert_equal expected_dx actual_dx ~printer:string_of_int;
  assert_equal expected_dy actual_dy ~printer:string_of_int

(** [test_of_key name key expected_direction] is a test case with [name] that
    checks if mapping [key] gives [expected_direction]. *)
let test_of_key name key expected_direction =
  name >:: fun _ ->
  assert_equal expected_direction (of_key key) ~printer:string_of_option

let tests =
  "test suite"
  >::: [
         test_current_x "player at origin has x=0" (create_player 0 0 10 10) 0;
         test_current_x "player at positive x=3" (create_player 3 1 10 10) 3;
         test_current_x "player at negative x=-10"
           (create_player (-10) 0 10 10)
           (-10);
         test_current_x "player at large x=10000"
           (create_player 10000 0 10 10)
           10000;
         test_current_y "player at origin has y=0" (create_player 0 0 20 20) 0;
         test_current_y "player at positive y=46" (create_player 82 46 20 20) 46;
         test_current_y "player at negative y=-2"
           (create_player 0 (-2) 20 20)
           (-2);
         test_current_y "player at large y=10000"
           (create_player 0 10000 20 20)
           10000;
         test_get_height "player with height 1" (create_player 0 2 1 2) 1;
         test_get_height "player with height 100"
           (create_player 4 92 100 22)
           100;
         test_get_height "player with height 0 (invalid)"
           (create_player 4 92 0 22) 0;
         test_get_width "player with width 22" (create_player 4 92 28 22) 22;
         test_get_width "player with width 50" (create_player 4 92 28 50) 50;
         test_get_width "player with width 0 (invalid)"
           (create_player 4 92 28 0) 0;
         test_move_player "move player by (0,0) does not change position"
           (create_player 5 5 10 10) 0 0 5 5;
         test_move_player "move player by (2,3)" (create_player 1 1 10 10) 2 3 3
           4;
         test_move_player "move player by (-2,-3)" (create_player 5 5 10 10)
           (-2) (-3) 3 2;
         test_move_player "move player from origin to negative position"
           (create_player 0 0 10 10) (-5) (-5) (-5) (-5);
         test_move_player "move player to large positive coordinates"
           (create_player 0 0 10 10) 1000 1000 1000 1000;
         test_get_corners "player at (0,0) with height 10 and width 20"
           (create_player 0 0 10 20) 10 20;
         test_get_corners "player at (5,5) with height 15 and width 25"
           (create_player 5 5 15 25) 20 30;
         test_get_corners "player at (-5,-5) with height 10 and width 20"
           (create_player (-5) (-5) 10 20)
           5 15;
         test_get_corners "player at large position with large dimensions"
           (create_player 1000 2000 500 500)
           1500 2500;
         test_create_proj "create projectile at origin with zero velocity" 0 0 0
           0 0 0 0 0;
         test_create_proj "create projectile at positive position" 10 20 5 5 10
           20 5 5;
         test_create_proj "create projectile at negative position" (-10) (-20)
           (-5) (-5) (-10) (-20) (-5) (-5);
         test_create_proj "create projectile with large values" 10000 20000 50
           50 10000 20000 50 50;
         test_move_proj "move projectile from origin with velocity (0,0)"
           (create_proj 0 0 0 0) 0 0;
         test_move_proj "move projectile with positive velocity"
           (create_proj 10 20 5 5) 15 25;
         test_move_proj "move projectile with negative velocity"
           (create_proj 10 20 (-5) (-5))
           5 15;
         test_move_proj "move projectile with mixed velocity"
           (create_proj 10 20 5 (-5)) 15 15;
         test_in_bounds "projectile at origin within small screen"
           (create_proj 0 0 0 0) 10 10 true;
         test_in_bounds "projectile at positive position within bounds"
           (create_proj 5 5 0 0) 10 10 true;
         test_in_bounds "projectile at edge of bounds" (create_proj 10 10 0 0)
           10 10 false;
         test_in_bounds "projectile out of bounds" (create_proj 11 11 0 0) 10 10
           false;
         test_in_bounds "negative projectile out of bounds"
           (create_proj (-1) (-1) 0 0)
           10 10 false;
         test_in_bounds "large projectile within large bounds"
           (create_proj 1000 1000 0 0)
           2000 2000 true;
         test_get_position "get position of projectile at origin"
           (create_proj 0 0 0 0) 0 0;
         test_get_position "get position of projectile at positive coordinates"
           (create_proj 10 20 5 5) 10 20;
         test_get_position "get position of projectile at negative coordinates"
           (create_proj (-10) (-20) (-5) (-5))
           (-10) (-20);
         test_get_position "get position of projectile with large values"
           (create_proj 10000 20000 50 50)
           10000 20000;
         test_to_delta "to_delta for up" up 0 5;
         test_to_delta "to_delta for down" down 0 (-5);
         test_to_delta "to_delta for left" left (-5) 0;
         test_to_delta "to_delta for right" right 5 0;
         test_of_key "of_key for 'w'" 'w' (Some up);
         test_of_key "of_key for 's'" 's' (Some down);
         test_of_key "of_key for 'a'" 'a' (Some left);
         test_of_key "of_key for 'd'" 'd' (Some right);
         test_of_key "of_key for invalid key 'x'" 'x' None;
         test_of_key "of_key for numeric key '1'" '1' None;
         test_of_key "of_key for special character '@'" '@' None;
       ]

let _ = run_test_tt_main tests
