open OUnit2 
open Object
open Actor

let add = Adder 10
let mult = Multiplier 10
let phas = Phaser 10
let slow = Slower 10

let smallb = SmallB
let largeb = LargeB
let goodb = GoodB Nothing
let addb = GoodB add
let multb = GoodB mult
let phasb = GoodB phas
let slowb = GoodB slow

let actor_tests = [
  "effect of smallb" >:: (fun _ -> 
      assert_raises (Failure "No effect")
        (fun () -> get_effect smallb));
  "effect of largeb" >:: (fun _ -> 
      assert_raises (Failure "No effect")
        (fun () -> get_effect largeb));
  "effect of goodb" >:: (fun _ -> 
      assert_equal (Nothing) (get_effect goodb));
  "effect of addb" >:: (fun _ -> 
      assert_equal (add) (get_effect addb));
  "effect of multb" >:: (fun _ -> 
      assert_equal (mult) (get_effect multb));
  "effect of phasb" >:: (fun _ -> 
      assert_equal (phas) (get_effect phasb));
  "effect of slowb" >:: (fun _ -> 
      assert_equal (slow) (get_effect slowb));
  "time of smallb" >:: (fun _ -> 
      assert_raises (Failure "No effect")
        (fun () -> get_time smallb));
  "time of largeb" >:: (fun _ -> 
      assert_raises (Failure "No effect")
        (fun () -> get_time largeb));
  "time of goodb" >:: (fun _ -> 
      assert_equal 0 (get_time goodb));
  "time of addb" >:: (fun _ -> 
      assert_equal 0 (get_time addb));
  "time of multb" >:: (fun _ -> 
      assert_equal 10 (get_time multb));
  "time of phasb" >:: (fun _ -> 
      assert_equal 10 (get_time phasb));
  "time of slowb" >:: (fun _ -> 
      assert_equal 10 (get_time slowb));
  "is_good of smallb" >:: (fun _ -> 
      assert (not (is_good smallb)));
  "is_good of largeb" >:: (fun _ -> 
      assert (not (is_good largeb)));
  "is_good of goodb" >:: (fun _ -> 
      assert (is_good goodb));
  "is_good of phasb" >:: (fun _ -> 
      assert (is_good phasb));
]

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
    effects = []
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
      effects = []
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
      effects = []
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
      effects = []
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
      effects = []
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
      effects = []
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
      effects = []
    })

let empty_col_row = Generator.generate 10 10 100 1 1

let non_empty_row = Generator.generate 2 4 0 1 1 

let collision_tests = [

  "same loc: collision" >:: (fun _ -> 
      assert_equal 
        (Some LargeB) (Object.check_collision player enemy_same));

  "enemy to left: collision" >:: (fun _ -> 
      assert_equal  
        (Some LargeB) (Object.check_collision player enemy_left));

  "enemy to right: collision" >:: (fun _ -> 
      assert_equal  
        (Some LargeB) (Object.check_collision player enemy_right));

  "enemy above: no collision" >:: (fun _ -> 
      assert_equal 
        None (Object.check_collision player enemy_above));

  "enemy to left: no collision" >:: (fun _ -> 
      assert_equal  
        None (Object.check_collision player enemy_left2));

  "enemy to right: no collision" >:: (fun _ -> 
      assert_equal  
        None (Object.check_collision player enemy_right2));
]

let generator_tests = [
  "empty row" >:: (fun _ -> 
      assert_equal 
        [] empty_col_row);
  "non-empty row" >:: (fun _ ->
      assert (non_empty_row <> []))
]

let suite =
  "test suite for Crossy Caml"  >::: List.flatten [
    collision_tests;
    generator_tests;
    actor_tests;
  ]

let _ = run_test_tt_main suite