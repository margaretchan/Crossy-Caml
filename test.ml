open OUnit2 

let player = 
  Object.Player {
    x_pos = 5; 
    y_pos = 5;
    velocity = No, 0 ;
    id = 0;
    to_kill = false;
    score = 0;
    height = 5; 
    width = 5;
  }

let enemy_same = 
  Object.Block (LargeB, {
      x_pos = 5; 
      y_pos = 4;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 5;
    })

let enemy_left = 
  Object.Block (LargeB, {
      x_pos = 1; 
      y_pos = 4;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 5;
    })

let enemy_right = 
  Object.Block (LargeB, {
      x_pos = 7; 
      y_pos = 4;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 5;
    })

let enemy_above = 
  Object.Block (LargeB, {
      x_pos = 5; 
      y_pos = 11;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 5;
    })


let enemy_left2 = 
  Object.Block (LargeB, {
      x_pos = 0; 
      y_pos = 11;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 5;
    })

let enemy_right2 = 
  Object.Block (LargeB, {
      x_pos = 10; 
      y_pos = 11;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 5;
    })


let collision_tests = [

  "same loc: collision" >:: (fun _ -> 
      assert_equal  ~printer:(string_of_bool)
        true (Object.check_collision player enemy_same));

  "enemy to left: collision" >:: (fun _ -> 
      assert_equal  ~printer:(string_of_bool)
        true (Object.check_collision player enemy_left));

  "enemy to right: collision" >:: (fun _ -> 
      assert_equal  ~printer:(string_of_bool)
        true (Object.check_collision player enemy_right));

  "enemy above: no collision" >:: (fun _ -> 
      assert_equal  ~printer:(string_of_bool)
        false (Object.check_collision player enemy_above));

  "enemy to left: no collision" >:: (fun _ -> 
      assert_equal  ~printer:(string_of_bool)
        false (Object.check_collision player enemy_left2));

  "enemy to right: no collision" >:: (fun _ -> 
      assert_equal  ~printer:(string_of_bool)
        false (Object.check_collision player enemy_right2));
]

let suite =
  "test suite for Crossy Caml"  >::: List.flatten [
    collision_tests;
  ]

let _ = run_test_tt_main suite