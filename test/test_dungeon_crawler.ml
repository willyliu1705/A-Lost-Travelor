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

let tests =
  "test suite"
  >::: [
         test_current_x "player centered at (0,0) has x=0"
           (create_player 0 0 10 10) 0;
         test_current_y "player centered at (0,0) has y=0"
           (create_player 0 0 20 20) 0;
         test_current_x "player centered at (3,1) has x=3"
           (create_player 3 1 10 10) 3;
         test_current_y "player centered at (82,46) has y=46"
           (create_player 82 46 20 20)
           46;
         test_current_x "player centered at (-10,0) has x=-10"
           (create_player (-10) 0 10 10)
           (-10);
         test_current_y "player centered at (0,-2) has y=-2"
           (create_player 0 (-2) 20 20)
           (-2);
         test_get_height "player with height 1" (create_player 0 2 1 2) 1;
         test_get_width "player with width 22" (create_player 4 92 28 22) 22;
       ]

let _ = run_test_tt_main tests
