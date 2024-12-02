open OUnit2
open Dungeon_crawler.Player

(** [test_current_x name input expected_output] is a test case with [name] and
    checks if the current x of the player [input] is equal to [expected_output]. *)
let test_current_x name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (current_x_pos input) ~printer:string_of_int

(** [test_current_y name input expected_output] is a test case with [name] and
    checks if the current y of the player [input] is equal to [expected_output]. *)
let test_current_y name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (current_y_pos input) ~printer:string_of_int

(** [test_get_height name input expected_ouput] is a test case with [name] and
    checks if the current height of the player [input] is equal to
    [expected_output]. *)
let test_get_height name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_height input) ~printer:string_of_int

(** [test_get_width name input expected_ouput] is a test case with [name] and
    checks if the current width of the player [input] is equal to
    [expected_output]. *)
let test_get_width name input expected_output =
  name >:: fun _ ->
  assert_equal expected_output (get_width input) ~printer:string_of_int

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
       ]

let _ = run_test_tt_main tests
