open OUnit2
open Dungeon_crawler.Player
open Dungeon_crawler.Projectile
open Dungeon_crawler.Direction
open Dungeon_crawler.Enemy

let string_of_tuple (x, y) = Printf.sprintf "(%d, %d)" x y

let string_of_option = function
  | Some _ -> "Some direction"
  | None -> "None"

(** [test_current_x name input expected_output] is a test case with [name] that
    checks if the current x of the player [input] is equal to [expected_x]. *)
let test_current_x name input expected_x =
  name >:: fun _ ->
  assert_equal expected_x (current_x_pos input) ~printer:string_of_int

(** [test_current_y name input expected_output] is a test case with [name] that
    checks if the current y of the player [input] is equal to [expected_y]. *)
let test_current_y name input expected_y =
  name >:: fun _ ->
  assert_equal expected_y (current_y_pos input) ~printer:string_of_int

(** [test_get_height name input expected_ouput] is a test case with [name] that
    checks if the current height of the player [input] is equal to
    [expected_height]. *)
let test_get_height name input expected_height =
  name >:: fun _ ->
  assert_equal expected_height (get_height input) ~printer:string_of_int

(** [test_get_width name input expected_ouput] is a test case with [name] that
    checks if the current width of the player [input] is equal to
    [expected_width]. *)
let test_get_width name input expected_width =
  name >:: fun _ ->
  assert_equal expected_width (get_width input) ~printer:string_of_int

(** [test_move_player_x name player dx dy expected_x] is a test case with [name]
    that checks if moving [player] by ([dx], [dy]) results in the expected
    x-position [expected_x]. *)
let test_move_player_x name player dx dy expected_x =
  name >:: fun _ ->
  let () = move_player player dx dy in
  assert_equal expected_x (current_x_pos player) ~printer:string_of_int

(** [test_move_player_y name player dx dy expected_y] is a test case with [name]
    that checks if moving [player] by ([dx], [dy]) results in the expected
    y-position [expected_y]. *)
let test_move_player_y name player dx dy expected_y =
  name >:: fun _ ->
  let () = move_player player dx dy in
  assert_equal expected_y (current_y_pos player) ~printer:string_of_int

(** [test_get_corners_height name player expected_height] is a test case with
    [name] that checks if calling [get_corners] on [player] results in the
    correct height [expected_height]. *)
let test_get_corners_height name player expected_height =
  name >:: fun _ ->
  let corners = get_corners player in
  assert_equal expected_height (get_height corners) ~printer:string_of_int

(** [test_get_corners_width name player expected_width] is a test case with
    [name] that checks if calling [get_corners] on [player] results in the
    correct width [expected_width]. *)
let test_get_corners_width name player expected_width =
  name >:: fun _ ->
  let corners = get_corners player in
  assert_equal expected_width (get_width corners) ~printer:string_of_int

(** [test_create_proj_position name x y dx dy expected_x expected_y] is a test
    case with [name] that checks if creating a projectile results in the
    expected position ([expected_x], [expected_y]). *)
let test_create_proj_position name x y dx dy expected_x expected_y =
  name >:: fun _ ->
  let proj = create_proj x y dx dy in
  assert_equal (expected_x, expected_y) (get_proj_position proj)
    ~printer:string_of_tuple

(** [test_create_proj_velocity name x y dx dy expected_dx expected_dy] is a test
    case with [name] that checks if creating a projectile results in the
    expected velocity ([expected_dx], [expected_dy]). *)
let test_create_proj_velocity name x y dx dy expected_dx expected_dy =
  name >:: fun _ ->
  assert_equal (expected_dx, expected_dy) (dx, dy) ~printer:string_of_tuple

(** [test_move_proj name proj expected_x expected_y] is a test case with [name]
    that checks if moving the projectile [proj] results in the expected position
    ([expected_x], [expected_y]). *)
let test_move_proj name proj expected_x expected_y =
  name >:: fun _ ->
  let moved_proj = move_proj proj in
  assert_equal (expected_x, expected_y)
    (get_proj_position moved_proj)
    ~printer:string_of_tuple

(** [test_in_bounds name proj width height expected_bool] is a test case with
    [name] that checks if the projectile [proj] is within the bounds defined by
    ([width], [height]). *)
let test_in_bounds name proj width height expected_bool =
  name >:: fun _ ->
  assert_equal expected_bool
    (in_bounds proj width height)
    ~printer:string_of_bool

(** [test_get_position_x name proj expected_x] is a test case with [name] that
    checks if the x-position of [proj] is [expected_x]. *)
let test_get_position_x name proj expected_x =
  name >:: fun _ ->
  let actual_x, _ = get_proj_position proj in
  assert_equal expected_x actual_x ~printer:string_of_int

(** [test_get_position_y name proj expected_y] is a test case with [name] that
    checks if the y-position of [proj] is [expected_y]. *)
let test_get_position_y name proj expected_y =
  name >:: fun _ ->
  let _, actual_y = get_proj_position proj in
  assert_equal expected_y actual_y ~printer:string_of_int

(** [test_to_player_delta_dx name dir expected_dx] is a test case with [name]
    that checks if converting [dir] to a delta gives the expected x-coordinate
    [expected_dx]. *)
let test_to_player_delta_dx name dir expected_dx =
  name >:: fun _ ->
  let actual_dx, _ = to_player_delta dir in
  assert_equal expected_dx actual_dx ~printer:string_of_int

(** [test_to_player_delta_dy name dir expected_dy] is a test case with [name]
    that checks if converting [dir] to a delta gives the expected y-coordinate
    [expected_dy]. *)
let test_to_player_delta_dy name dir expected_dy =
  name >:: fun _ ->
  let _, actual_dy = to_player_delta dir in
  assert_equal expected_dy actual_dy ~printer:string_of_int

(** [test_to_player_projectile_delta_dx name direction expected_dx] is a test
    case with [name] that checks if converting [direction] to a projectile delta
    gives the expected x-coordinate [expected_dx]. *)
let test_to_player_projectile_delta_dx name direction expected_dx =
  name >:: fun _ ->
  let actual_dx, _ = to_player_projectile_delta direction in
  assert_equal expected_dx actual_dx ~printer:string_of_int

(** [test_to_player_projectile_delta_dy name direction expected_dy] is a test
    case with [name] that checks if converting [direction] to a projectile delta
    gives the expected y-coordinate [expected_dy]. *)
let test_to_player_projectile_delta_dy name direction expected_dy =
  name >:: fun _ ->
  let _, actual_dy = to_player_projectile_delta direction in
  assert_equal expected_dy actual_dy ~printer:string_of_int

(** [test_of_key name key expected_direction] is a test case with [name] that
    checks if mapping [key] gives [expected_direction]. *)
let test_of_key name key expected_direction =
  name >:: fun _ ->
  assert_equal expected_direction (of_key key) ~printer:string_of_option

(** [test_enemy_x_pos name input expected_x] is a test case with [name] that
    checks if the x-coordinate of [enemy] matches [expected_x]. *)
let test_enemy_x_pos name enemy expected_x =
  name >:: fun _ ->
  assert_equal expected_x (enemy_x_pos enemy) ~printer:string_of_int

(** [test_enemy_y_pos name input expected_y] is a test case with [name] that
    checks if the y-coordinate of [enemy] matches [expected_y]. *)
let test_enemy_y_pos name enemy expected_y =
  name >:: fun _ ->
  assert_equal expected_y (enemy_y_pos enemy) ~printer:string_of_int

(** [test_get_enemy_height name input expected_height] is a test case with
    [name] that checks if the height of [enemy] matches [expected_height]. *)
let test_get_enemy_height name enemy expected_height =
  name >:: fun _ ->
  assert_equal expected_height (get_enemy_height enemy) ~printer:string_of_int

(** [test_get_enemy_width name input expected_width] is a test case with [name]
    that checks if the width of [enemy] matches [expected_width]. *)
let test_get_enemy_width name enemy expected_width =
  name >:: fun _ ->
  assert_equal expected_width (get_enemy_width enemy) ~printer:string_of_int

(** [test_get_direction name enemy expected_dir] is a test case with [name] that
    checks if the direction of [enemy] matches [expected_dir]. *)
let test_get_direction name enemy expected_dir =
  name >:: fun _ ->
  assert_equal expected_dir (get_direction enemy) ~printer:direction_to_string

(** [test_set_direction name enemy new_dir] is a test case with [name] that
    checks if setting the direction of [enemy] to [new_dir] correctly updates
    the direction. *)
let test_set_direction name enemy new_dir =
  name >:: fun _ ->
  let () = set_direction enemy new_dir in
  assert_equal new_dir (get_direction enemy) ~printer:direction_to_string

(** [test_aligned_with_player name enemy px py expected] is a test case with
    [name] that checks if the enemy is aligned with the player at ([px], [py]). *)
let test_aligned_with_player name enemy px py expected =
  name >:: fun _ ->
  assert_equal expected
    (aligned_with_player enemy (px, py))
    ~printer:string_of_bool

(** [test_create_enemy_with_speed name x y w h dir speed expected_speed] is a
    test case with [name] that checks if creating an enemy with the given
    parameters results in an enemy with the expected projectile speed. *)
let test_create_enemy_with_speed name x y w h dir speed delay expected_speed =
  name >:: fun _ ->
  let enemy = create_enemy x y w h dir speed delay in
  assert_equal expected_speed
    (get_projectile_speed enemy)
    ~printer:string_of_int

(** [test_is_up name dir expected] is a test case with [name] that checks if
    [is_up dir] returns [expected]. *)
let test_is_up name dir expected =
  name >:: fun _ -> assert_equal expected (is_up dir) ~printer:string_of_bool

(** [test_is_down name dir expected] is a test case with [name] that checks if
    [is_down dir] returns [expected]. *)
let test_is_down name dir expected =
  name >:: fun _ -> assert_equal expected (is_down dir) ~printer:string_of_bool

(** [test_is_left name dir expected] is a test case with [name] that checks if
    [is_left dir] returns [expected]. *)
let test_is_left name dir expected =
  name >:: fun _ -> assert_equal expected (is_left dir) ~printer:string_of_bool

(** [test_is_right name dir expected] is a test case with [name] that checks if
    [is_right dir] returns [expected]. *)
let test_is_right name dir expected =
  name >:: fun _ -> assert_equal expected (is_right dir) ~printer:string_of_bool

(** [test_create_enemy_with_delay name x y w h dir speed delay expected_delay]
    is a test case with [name] that checks if creating an enemy with the given
    parameters results in an enemy with the expected shooting delay. *)
let test_create_enemy_with_delay name x y w h dir speed delay expected_delay =
  name >:: fun _ ->
  let enemy = create_enemy x y w h dir speed delay in
  assert_equal expected_delay (get_shooting_delay enemy)
    ~printer:string_of_float

(** [test_enemy_shoot_with_delay name enemy projectiles_ref last_shot_time_ref
    current_time expected_projectile_count]
    is a test case with [name] that checks if the enemy correctly uses its
    shooting delay when firing projectiles. *)
let test_enemy_shoot_with_delay name enemy projectiles_ref last_shot_time_ref
    current_time expected_projectile_count =
  name >:: fun _ ->
  let () =
    enemy_shoot enemy projectiles_ref last_shot_time_ref
      (get_shooting_delay enemy) current_time
  in
  assert_equal expected_projectile_count
    (List.length !projectiles_ref)
    ~printer:string_of_int

(** [test_handle_collision name projectiles_ref x y w h expected_count] is a
    test case with [name] that checks if [handle_collision] removes the correct
    number of colliding projectiles from [projectiles_ref]. *)
let test_handle_collision name projectiles_ref x y w h expected_count =
  name >:: fun _ ->
  handle_collision projectiles_ref x y w h;
  assert_equal expected_count
    (List.length !projectiles_ref)
    ~printer:string_of_int

(** [test_detect_collision name projectiles_ref x y w h expected] is a test case
    with [name] that checks if [detect_collision] correctly detects collisions
    for the projectiles in [projectiles_ref]. *)
let test_detect_collision name projectiles_ref x y w h expected =
  name >:: fun _ ->
  assert_equal expected
    (detect_collision projectiles_ref x y w h)
    ~printer:string_of_bool

let tests =
  "test suite"
  >::: [
         test_current_x "player at origin has x=0" (create_player 0 0 10 10) 0;
         test_current_x "player at positive x=3" (create_player 3 1 10 10) 3;
         test_current_x "player at negative x=-10"
           (create_player (-10) 0 10 10)
           (-10);
         test_current_x "player at large positive x=10000"
           (create_player 10000 0 10 10)
           10000;
         test_current_x "player at large negative x=-10000"
           (create_player (-10000) 0 10 10)
           (-10000);
         test_current_y "player at origin has y=0" (create_player 0 0 20 20) 0;
         test_current_y "player at positive y=46" (create_player 82 46 20 20) 46;
         test_current_y "player at negative y=-2"
           (create_player 0 (-2) 20 20)
           (-2);
         test_current_y "player at large positive y=10000"
           (create_player 0 10000 20 20)
           10000;
         test_current_y "player at large negative y=-10000"
           (create_player 0 (-10000) 20 20)
           (-10000);
         test_get_height "player with height 1" (create_player 0 2 2 1) 1;
         test_get_height "player with height 100"
           (create_player 4 92 22 100)
           100;
         test_get_height "player with height 0 (invalid)"
           (create_player 4 92 22 0) 0;
         test_get_width "player with width 22" (create_player 4 92 22 28) 22;
         test_get_width "player with width 50" (create_player 4 92 50 28) 50;
         test_get_width "player with width 0 (invalid)"
           (create_player 4 92 0 28) 0;
         test_move_player_x "move player by (0,0) does not change x"
           (create_player 5 5 10 10) 0 0 5;
         test_move_player_y "move player by (0,0) does not change y"
           (create_player 5 5 10 10) 0 0 5;
         test_move_player_x "move player by (2,3) updates x correctly"
           (create_player 1 1 10 10) 2 3 3;
         test_move_player_y "move player by (2,3) updates y correctly"
           (create_player 1 1 10 10) 2 3 4;
         test_move_player_x "move player by very small positive dx"
           (create_player 0 0 10 10) 1 0 1;
         test_move_player_y "move player by very small positive dy"
           (create_player 0 0 10 10) 0 1 1;
         test_move_player_x "move player by very small negative dx"
           (create_player 2 2 10 10) (-1) 0 1;
         test_move_player_y "move player by very small negative dy"
           (create_player 3 3 10 10) 0 (-1) 2;
         test_move_player_x "move player to large positive x-coordinate"
           (create_player 0 0 10 10) 1000 0 1000;
         test_move_player_y "move player to large positive y-coordinate"
           (create_player 0 0 10 10) 0 1000 1000;
         test_move_player_x "move player to negative x-coordinate"
           (create_player 0 0 10 10) (-5) 0 (-5);
         test_move_player_y "move player to negative y-coordinate"
           (create_player 0 0 10 10) 0 (-5) (-5);
         test_move_player_x "move player by large negative x-coordinate"
           (create_player 1000 1000 10 10)
           (-2000) 0 (-1000);
         test_move_player_y "move player by large negative y-coordinate"
           (create_player 1000 1000 10 10)
           0 (-2000) (-1000);
         test_move_player_x "move player by very large positive dx"
           (create_player 0 0 10 10) max_int 0 max_int;
         test_move_player_y "move player by very large positive dy"
           (create_player 0 0 10 10) 0 max_int max_int;
         test_move_player_x "move player by very large negative dx"
           (create_player max_int max_int 10 10)
           (-max_int) 0 0;
         test_move_player_y "move player by very large negative dy"
           (create_player max_int max_int 10 10)
           0 (-max_int) 0;
         test_get_corners_height "get corners of player updates height"
           (create_player 0 0 20 10) 10;
         test_get_corners_width "get corners of player updates width"
           (create_player 0 0 20 10) 20;
         test_get_corners_height "get corners of large player updates height"
           (create_player 1000 2000 500 500)
           1500;
         test_get_corners_width "get corners of large player updates width"
           (create_player 1000 2000 500 500)
           2500;
         test_get_corners_height
           "get corners of negative position updates height"
           (create_player (-10) (-20) 15 25)
           15;
         test_get_corners_width "get corners of negative position updates width"
           (create_player (-10) (-20) 15 25)
           (-5);
         test_get_corners_height "get corners with very large height"
           (create_player 0 0 20 max_int)
           max_int;
         test_get_corners_width "get corners with very large width"
           (create_player 0 0 max_int 10)
           max_int;
         test_get_corners_height "get corners with zero height"
           (create_player 0 0 20 0) 0;
         test_get_corners_width "get corners with zero width"
           (create_player 0 0 0 10) 0;
         test_create_proj_position
           "create projectile at origin with zero velocity" 0 0 0 0 0 0;
         test_create_proj_velocity "create projectile with positive velocity" 10
           20 5 5 5 5;
         test_create_proj_position
           "create projectile with large position values" 10000 20000 50 50
           10000 20000;
         test_create_proj_velocity
           "create projectile with large velocity values" 10000 20000 1000 1000
           1000 1000;
         test_create_proj_position "create projectile at negative position"
           (-10) (-20) (-5) (-5) (-10) (-20);
         test_create_proj_velocity "create projectile with negative velocity"
           (-10) (-20) (-5) (-5) (-5) (-5);
         test_create_proj_position
           "create projectile at very large position with zero velocity" max_int
           max_int 0 0 max_int max_int;
         test_create_proj_velocity
           "create projectile with very large positive velocity" 0 0 max_int
           max_int max_int max_int;
         test_create_proj_velocity
           "create projectile with very large negative velocity" 0 0 (-max_int)
           (-max_int) (-max_int) (-max_int);
         test_create_proj_velocity "create projectile with dx=0 and dy=non-zero"
           10 20 0 3 0 3;
         test_create_proj_velocity "create projectile with dx=non-zero and dy=0"
           10 20 (-3) 0 (-3) 0;
         test_create_proj_position
           "create projectile with mixed large positive and negative position"
           max_int (-max_int) 0 0 max_int (-max_int);
         test_create_proj_position "create projectile with mixed dx and dy" 10
           20 3 (-4) 10 20;
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
         test_in_bounds "projectile far out of bounds"
           (create_proj 1000 1000 0 0)
           10 10 false;
         test_in_bounds "negative projectile out of bounds"
           (create_proj (-1) (-1) 0 0)
           10 10 false;
         test_in_bounds "large projectile within large bounds"
           (create_proj 1000 1000 0 0)
           2000 2000 true;
         test_in_bounds "projectile exactly at top-left corner"
           (create_proj 0 0 0 0) 10 10 true;
         test_in_bounds "projectile exactly at bottom-right corner"
           (create_proj 10 10 0 0) 10 10 false;
         test_in_bounds "projectile slightly outside top-left corner"
           (create_proj (-1) (-1) 0 0)
           10 10 false;
         test_in_bounds "projectile slightly outside bottom-right corner"
           (create_proj 11 11 0 0) 10 10 false;
         test_in_bounds "projectile within a very large screen"
           (create_proj 10000 10000 0 0)
           max_int max_int true;
         test_get_position_x "get x-position of projectile at origin"
           (create_proj 0 0 0 0) 0;
         test_get_position_y "get y-position of projectile at origin"
           (create_proj 0 0 0 0) 0;
         test_get_position_x
           "get x-position of projectile at positive coordinates"
           (create_proj 10 20 5 5) 10;
         test_get_position_y
           "get y-position of projectile at positive coordinates"
           (create_proj 10 20 5 5) 20;
         test_get_position_x
           "get x-position of projectile at negative coordinates"
           (create_proj (-10) (-20) (-5) (-5))
           (-10);
         test_get_position_y
           "get y-position of projectile at negative coordinates"
           (create_proj (-10) (-20) (-5) (-5))
           (-20);
         test_get_position_x "get x-position of projectile at max_int"
           (create_proj max_int 0 0 0)
           max_int;
         test_get_position_y "get y-position of projectile at max_int"
           (create_proj 0 max_int 0 0)
           max_int;
         test_get_position_x "get x-position of projectile at -max_int"
           (create_proj (-max_int) 0 0 0)
           (-max_int);
         test_get_position_y "get y-position of projectile at -max_int"
           (create_proj 0 (-max_int) 0 0)
           (-max_int);
         test_to_player_delta_dx "to_delta for up gives dx=0" up 0;
         test_to_player_delta_dy "to_delta for up gives dy=player_speed" up
           player_speed;
         test_to_player_delta_dx "to_delta for down gives dx=0" down 0;
         test_to_player_delta_dy "to_delta for down gives dy=-player_speed" down
           (-player_speed);
         test_to_player_delta_dx "to_delta for left gives dx=-player_speed" left
           (-player_speed);
         test_to_player_delta_dy "to_delta for left gives dy=0" left 0;
         test_to_player_delta_dx "to_delta for right gives dx=player_speed"
           right player_speed;
         test_to_player_delta_dy "to_delta for right gives dy=0" right 0;
         test_to_player_projectile_delta_dx
           "to_projectile_delta for up gives dx=0" up 0;
         test_to_player_projectile_delta_dy
           "to_projectile_delta for up gives dy=projectile_speed" up
           player_projectile_speed;
         test_to_player_projectile_delta_dx
           "to_projectile_delta for down gives dx=0" down 0;
         test_to_player_projectile_delta_dy
           "to_projectile_delta for down gives dy=-projectile_speed" down
           (-player_projectile_speed);
         test_to_player_projectile_delta_dx
           "to_projectile_delta for left gives dx=-projectile_speed" left
           (-player_projectile_speed);
         test_to_player_projectile_delta_dy
           "to_projectile_delta for left gives dy=0" left 0;
         test_to_player_projectile_delta_dx
           "to_projectile_delta for right gives dx=projectile_speed" right
           player_projectile_speed;
         test_to_player_projectile_delta_dy
           "to_projectile_delta for right gives dy=0" right 0;
         test_of_key "of_key for 'w'" 'w' (Some up);
         test_of_key "of_key for 's'" 's' (Some down);
         test_of_key "of_key for 'a'" 'a' (Some left);
         test_of_key "of_key for 'd'" 'd' (Some right);
         test_of_key "of_key for invalid key 'x'" 'x' None;
         test_of_key "of_key for numeric key '1'" '1' None;
         test_of_key "of_key for special character '@'" '@' None;
         test_of_key "of_key for empty character" '\000' None;
         test_of_key "of_key for extended ASCII character" '\255' None;
         test_of_key "of_key for uppercase 'W'" 'W' None;
         test_of_key "of_key for lowercase 'z'" 'z' None;
         test_of_key "of_key for unsupported special character '*'" '*' None;
         test_enemy_x_pos "enemy at origin has x=0"
           (create_enemy 0 0 10 10 up 1 1.0)
           0;
         test_enemy_x_pos "enemy at positive x=50"
           (create_enemy 50 0 10 10 up 1 1.0)
           50;
         test_enemy_x_pos "enemy at negative x=-30"
           (create_enemy (-30) 0 10 10 up 1 1.0)
           (-30);
         test_enemy_x_pos "enemy at large positive x=10000"
           (create_enemy 10000 0 10 10 up 1 1.0)
           10000;
         test_enemy_x_pos "enemy at large negative x=-10000"
           (create_enemy (-10000) 0 10 10 up 1 1.0)
           (-10000);
         test_enemy_y_pos "enemy at origin has y=0"
           (create_enemy 0 0 10 10 up 1 1.0)
           0;
         test_enemy_y_pos "enemy at positive y=50"
           (create_enemy 0 50 10 10 up 1 1.0)
           50;
         test_enemy_y_pos "enemy at negative y=-30"
           (create_enemy 0 (-30) 10 10 up 1 1.0)
           (-30);
         test_enemy_y_pos "enemy at large positive y=10000"
           (create_enemy 0 10000 10 10 up 1 1.0)
           10000;
         test_enemy_y_pos "enemy at large negative y=-10000"
           (create_enemy 0 (-10000) 10 10 up 1 1.0)
           (-10000);
         test_get_enemy_height "enemy with height 10"
           (create_enemy 0 0 10 10 up 1 1.0)
           10;
         test_get_enemy_height "enemy with height 50"
           (create_enemy 0 0 10 50 up 1 1.0)
           50;
         test_get_enemy_height "enemy with height 0 (invalid)"
           (create_enemy 0 0 10 0 up 1 1.0)
           0;
         test_get_enemy_width "enemy with width 10"
           (create_enemy 0 0 10 10 up 1 1.0)
           10;
         test_get_enemy_width "enemy with width 50"
           (create_enemy 0 0 50 10 up 1 1.0)
           50;
         test_get_enemy_width "enemy with width 0 (invalid)"
           (create_enemy 0 0 0 10 up 1 1.0)
           0;
         test_get_direction "enemy facing up"
           (create_enemy 0 0 10 10 up 1 1.0)
           up;
         test_get_direction "enemy facing down"
           (create_enemy 0 0 10 10 down 1 1.0)
           down;
         test_get_direction "enemy facing left"
           (create_enemy 0 0 10 10 left 1 1.0)
           left;
         test_get_direction "enemy facing right"
           (create_enemy 0 0 10 10 right 1 1.0)
           right;
         test_set_direction "set enemy direction to up"
           (create_enemy 0 0 10 10 up 1 1.0)
           up;
         test_set_direction "set enemy direction to down"
           (create_enemy 0 0 10 10 up 1 1.0)
           down;
         test_set_direction "set enemy direction to left"
           (create_enemy 0 0 10 10 up 1 1.0)
           left;
         test_set_direction "set enemy direction to right"
           (create_enemy 0 0 10 10 up 1 1.0)
           right;
         test_aligned_with_player "player directly above enemy is aligned"
           (create_enemy 50 50 10 10 up 1 1.0)
           55 70 true;
         test_aligned_with_player
           "player not in line above enemy is not aligned"
           (create_enemy 50 50 10 10 up 1 1.0)
           45 70 false;
         test_aligned_with_player
           "player aligned with enemy facing up but far away"
           (create_enemy 50 50 10 10 up 1 1.0)
           55 500 true;
         test_aligned_with_player "player directly below enemy is aligned"
           (create_enemy 50 50 10 10 down 1 1.0)
           55 30 true;
         test_aligned_with_player
           "player not in line below enemy is not aligned"
           (create_enemy 50 50 10 10 down 1 1.0)
           65 30 false;
         test_aligned_with_player
           "player aligned with enemy facing down but far away"
           (create_enemy 50 50 10 10 down 1 1.0)
           55 (-500) true;
         test_aligned_with_player "player directly left of enemy is aligned"
           (create_enemy 50 50 10 10 left 1 1.0)
           40 55 true;
         test_aligned_with_player
           "player not in line left of enemy is not aligned"
           (create_enemy 50 50 10 10 left 1 1.0)
           40 65 false;
         test_aligned_with_player
           "player aligned with enemy facing left but far away"
           (create_enemy 50 50 10 10 left 1 1.0)
           (-500) 50 true;
         test_aligned_with_player "player directly right of enemy is aligned"
           (create_enemy 50 50 10 10 right 1 1.0)
           70 55 true;
         test_aligned_with_player
           "player not in line right of enemy is not aligned"
           (create_enemy 50 50 10 10 right 1 1.0)
           70 65 false;
         test_aligned_with_player
           "player aligned with enemy facing right but far away"
           (create_enemy 50 50 10 10 right 1 1.0)
           500 50 true;
         test_create_enemy_with_speed "Enemy with projectile speed 1" 10 10 20
           20 up 1 1.0 1;
         test_create_enemy_with_speed "Enemy with projectile speed 5" 30 30 40
           40 down 5 1.0 5;
         test_create_enemy_with_speed "Enemy with projectile speed 100" 50 50 60
           60 left 100 1.0 100;
         test_create_enemy_with_speed "Enemy with invalid projectile speed 0" 70
           70 80 80 right 0 1.0 0;
         test_is_up "is_up returns true for up" up true;
         test_is_up "is_up returns false for down" down false;
         test_is_up "is_up returns false for left" left false;
         test_is_up "is_up returns false for right" right false;
         test_is_down "is_down returns true for down" down true;
         test_is_down "is_down returns false for up" up false;
         test_is_down "is_down returns false for left" left false;
         test_is_down "is_down returns false for right" right false;
         test_is_left "is_left returns true for left" left true;
         test_is_left "is_left returns false for up" up false;
         test_is_left "is_left returns false for down" down false;
         test_is_left "is_left returns false for right" right false;
         test_is_right "is_right returns true for right" right true;
         test_is_right "is_right returns false for up" up false;
         test_is_right "is_right returns false for down" down false;
         test_is_right "is_right returns false for left" left false;
         test_create_enemy_with_delay "Enemy with delay 1.0" 0 0 10 10 up 1 1.0
           1.0;
         test_create_enemy_with_delay "Enemy with delay 2.0" 0 0 10 10 down 1
           2.5 2.5;
         test_create_enemy_with_delay "Enemy with large delay" 0 0 10 10 left 1
           100.0 100.0;
         test_enemy_shoot_with_delay "Enemy shoots after delay is satisfied"
           (create_enemy 50 50 10 10 up 1 1.5)
           (ref []) (ref 0.0) 1.5 1;
         test_enemy_shoot_with_delay "Enemy does not shoot before delay"
           (create_enemy 50 50 10 10 down 1 2.0)
           (ref []) (ref 0.0) 1.5 0;
         test_enemy_shoot_with_delay "Enemy with zero delay shoots instantly"
           (create_enemy 50 50 10 10 left 1 0.0)
           (ref []) (ref 0.0) 0.0 1;
         test_enemy_shoot_with_delay
           "Enemy does not shoot if delay is very high"
           (create_enemy 50 50 10 10 right 1 100.0)
           (ref []) (ref 0.0) 10.0 0;
         test_enemy_shoot_with_delay "Enemy shoots after very large delay"
           (create_enemy 50 50 10 10 down 1 500.0)
           (ref []) (ref 0.0) 500.0 1;
         test_enemy_shoot_with_delay
           "Enemy shoots after delay that does not end in 0 or 5"
           (create_enemy 50 50 10 10 right 1 1.2)
           (ref []) (ref 0.0) 1.4 1;
         test_handle_collision "No projectiles, no collision" (ref []) 0 0 10 10
           0;
         test_handle_collision "One projectile, no collision"
           (ref [ create_proj 20 20 0 0 ])
           0 0 10 10 1;
         test_handle_collision "One projectile, collision"
           (ref [ create_proj 5 5 0 0 ])
           0 0 10 10 0;
         test_handle_collision "Multiple projectiles, no collisions"
           (ref [ create_proj 20 20 0 0; create_proj 30 30 0 0 ])
           0 0 10 10 2;
         test_handle_collision "Multiple projectiles, some collisions"
           (ref [ create_proj 5 5 0 0; create_proj 20 20 0 0 ])
           0 0 10 10 1;
         test_handle_collision "Multiple projectiles, all collide"
           (ref [ create_proj 5 5 0 0; create_proj 8 8 0 0 ])
           0 0 10 10 0;
         test_handle_collision "Projectile at edge of rectangle (collision)"
           (ref [ create_proj 10 10 0 0 ])
           0 0 10 10 0;
         test_handle_collision "Projectile just inside rectangle (collision)"
           (ref [ create_proj 9 9 0 0 ])
           0 0 10 10 0;
         test_detect_collision "No projectiles, no collision" (ref []) 0 0 10 10
           false;
         test_detect_collision "One projectile, no collision"
           (ref [ create_proj 20 20 0 0 ])
           0 0 10 10 false;
         test_detect_collision "One projectile, collision"
           (ref [ create_proj 5 5 0 0 ])
           0 0 10 10 true;
         test_detect_collision "Multiple projectiles, no collisions"
           (ref [ create_proj 20 20 0 0; create_proj 30 30 0 0 ])
           0 0 10 10 false;
         test_detect_collision "Multiple projectiles, one collision"
           (ref [ create_proj 5 5 0 0; create_proj 20 20 0 0 ])
           0 0 10 10 true;
         test_detect_collision "Multiple projectiles, all collide"
           (ref [ create_proj 5 5 0 0; create_proj 8 8 0 0 ])
           0 0 10 10 true;
         test_detect_collision "Projectile at edge of rectangle (collision)"
           (ref [ create_proj 10 10 0 0 ])
           0 0 10 10 true;
         test_detect_collision "Projectile just inside rectangle (collision)"
           (ref [ create_proj 9 9 0 0 ])
           0 0 10 10 true;
       ]

let _ = run_test_tt_main tests
