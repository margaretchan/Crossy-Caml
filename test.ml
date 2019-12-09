open OUnit2 
open Object
open Actor
open Screen

let add = Adder 500
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
      assert_raises 
        (Failure "No effect") (fun () -> get_effect smallb));

  "effect of largeb" >:: (fun _ -> 
      assert_raises 
        (Failure "No effect") (fun () -> get_effect largeb));

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
      assert_raises 
        (Failure "No effect") (fun () -> get_time smallb));

  "time of largeb" >:: (fun _ -> 
      assert_raises 
        (Failure "No effect") (fun () -> get_time largeb));

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

let effect_list = [add; mult; phas; slow]

let player_obj = {
  x_pos = 5; 
  y_pos = 5;
  velocity = No, 0 ;
  id = 0;
  to_kill = false;
  score = 0;
  height = 5; 
  width = 5;
  effects = effect_list
}

let player_obj_empty = {
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

let player = 
  Object.Player player_obj

let new_effect_list = update_effects effect_list

let enemy_obj = {
  x_pos = 5; 
  y_pos = 4;
  velocity = No, 0 ;
  id = 0;
  to_kill = false;
  score = 0;
  height = 5; 
  width = 5;
  effects = []
}

let enemy_same_large = 
  Object.Block (LargeB, enemy_obj)

let enemy_same_small = 
  Object.Block (SmallB, {
      x_pos = 5; 
      y_pos = 4;
      velocity = No, 0 ;
      id = 0;
      to_kill = false;
      score = 0;
      height = 5; 
      width = 2;
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

let good_same = 
  Object.Block (GoodB (add), {
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

let good_left = 
  Object.Block (GoodB (add), {
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

let good_right = 
  Object.Block (GoodB (add), {
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

let player_off_screen = 
  Object.Player {
    x_pos = 5; 
    y_pos = -7;
    velocity = No, 0 ;
    id = 0;
    to_kill = false;
    score = 0;
    height = 5; 
    width = 5;
    effects = []
  }

let block_off_screen = 
  Object.Block (GoodB (add), {
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

let () = Object.score_incr player_obj 1 

let () = Object.score_incr player_obj_empty 1

let object_tests = [
  "get_block: LargeB" >:: (fun _ -> 
      assert_equal 
        (LargeB) (Object.get_block enemy_same_large));

  "get_block: Player" >:: (fun _ -> 
      assert_raises 
        (Failure "Not a block") (fun () -> get_block player));   

  "same loc with LargeB: collision" >:: (fun _ -> 
      assert_equal 
        (Some LargeB) (Object.check_collision player enemy_same_large));

  "same loc with SmallB: collision" >:: (fun _ -> 
      assert_equal 
        (Some SmallB) (Object.check_collision player enemy_same_small));

  "enemy to left: collision" >:: (fun _ -> 
      assert_equal  
        (Some LargeB) (Object.check_collision player enemy_left));

  "enemy to right: collision" >:: (fun _ -> 
      assert_equal  
        (Some LargeB) (Object.check_collision player enemy_right));

  "item to left: collision" >:: (fun _ -> 
      assert_equal  
        (Some (GoodB (add))) (Object.check_collision player good_left));

  "item to right: collision" >:: (fun _ -> 
      assert_equal  
        (Some (GoodB (add))) (Object.check_collision player good_right));

  "enemy above: no collision" >:: (fun _ -> 
      assert_equal 
        None (Object.check_collision player enemy_above));

  "enemy to left: no collision" >:: (fun _ -> 
      assert_equal  
        None (Object.check_collision player enemy_left2));

  "enemy to right: no collision" >:: (fun _ -> 
      assert_equal  
        None (Object.check_collision player enemy_right2));

  (* CHECK_ON_SCREEN TESTS BUT AREN'T USED CURRENTLY *)
  "extract_obj of player" >:: (fun _ -> 
      assert_equal  
        player_obj (Object.extract_obj player));

  "extract_obj of block" >:: (fun _ -> 
      assert_equal  
        enemy_obj (Object.extract_obj enemy_same_large));

  "has_phaser in complete effect list" >:: (fun _ -> 
      assert  
        (Object.has_phaser player_obj));

  "has_phaser in empty effect list" >:: (fun _ -> 
      assert  
        (not (Object.has_phaser player_obj_empty)));

  "has_slower in complete effect list" >:: (fun _ -> 
      assert
        (Object.has_slower player_obj));

  "has_slower in empty effect list" >:: (fun _ -> 
      assert  
        (not (Object.has_phaser player_obj_empty)));

  "has_mult in complete effect list" >:: (fun _ -> 
      assert
        (Object.has_mult player_obj));

  "has_mult in empty effect list" >:: (fun _ -> 
      assert
        (not (Object.has_mult player_obj_empty)));

  "has_adder in complete effect list" >:: (fun _ -> 
      assert
        (Object.has_adder player_obj));

  "has_adder in empty effect list" >:: (fun _ -> 
      assert
        (not (Object.has_adder player_obj_empty)));

  "score_incr on player with complete effect list" >:: (fun _ -> 
      assert_equal
        (45) (player_obj.score));

  "score_incr on player with empty effect list" >:: (fun _ -> 
      assert_equal
        (1) (player_obj_empty.score));

  "effect_time_left on adder in effect list" >:: (fun _ -> 
      assert_equal
        (500) (effect_time_left effect_list (add)));

  "effect_time_left on adder in updated list" >:: (fun _ -> 
      assert_equal
        (0) (effect_time_left new_effect_list (add)));

  "effect_time_left on mult in effect list" >:: (fun _ -> 
      assert_equal
        (10) (effect_time_left effect_list (mult)));

  "effect_time_left on mult in updated list" >:: (fun _ -> 
      assert_equal
        (9) (effect_time_left new_effect_list (mult)));

  "effect_time_left on phaser in effect list" >:: (fun _ -> 
      assert_equal
        (10) (effect_time_left effect_list (phas)));

  "effect_time_left on phaser in updated list" >:: (fun _ -> 
      assert_equal
        (9) (effect_time_left new_effect_list (phas)));

  "effect_time_left on slower in effect list" >:: (fun _ -> 
      assert_equal
        (10) (effect_time_left effect_list (slow)));

  "effect_time_left on adder in empty list" >:: (fun _ -> 
      assert_equal
        (9) (effect_time_left new_effect_list (slow)));
]

let gen_block = Generator.generate_block (0,0) 1 (GoodB add) Left 0   

let gen_block_type = Object.get_block gen_block

let gen_block_obj = Object.extract_obj gen_block

let empty_col_row = Generator.generate 10 10 100 1 1

let non_empty_row = Generator.generate 2 4 0 1 1 

let generator_tests = [
  "gen_block type test" >:: (fun _ -> 
      assert_equal 
        (GoodB add) gen_block_type);

  "gen_block field test: x_pos" >:: (fun _ -> 
      assert_equal 
        0 gen_block_obj.x_pos);

  "gen_block field test: y_pos" >:: (fun _ -> 
      assert_equal 
        0 gen_block_obj.y_pos);

  "gen_block field test: velocity (dir)" >:: (fun _ -> 
      assert_equal 
        Left (fst gen_block_obj.velocity));      

  "gen_block field test: velocity (spd)"  >:: (fun _ -> 
      assert_equal 
        0 (snd gen_block_obj.velocity));    

  "gen_block field test: id" >:: (fun _ -> 
      assert_equal 
        1 gen_block_obj.id);  

  "gen_block field test: to_kill" >:: (fun _ -> 
      assert
        (not (gen_block_obj.to_kill)));            

  "gen_block field test: score"  >:: (fun _ -> 
      assert_equal 
        40 gen_block_obj.score);

  "gen_block field test: height"  >:: (fun _ -> 
      assert_equal 
        2 gen_block_obj.height);            

  "gen_block field test: effects"  >:: (fun _ -> 
      assert_equal 
        [] gen_block_obj.effects);      

  "empty row" >:: (fun _ -> 
      assert_equal 
        [] empty_col_row);

  "non-empty row" >:: (fun _ ->
      assert 
        (non_empty_row <> []));

  "non-empty row length" >:: (fun _ ->
      assert_equal 
        1 (List.length non_empty_row));
]

let screen2 = Screen.empty 

let update_screen2 = Screen.update screen2 4 4 0 1 1 true

let update_screen2 = Screen.update screen2 10 4 2 1 1 true

let update_screen2= Screen.update screen2 4 4 100 1 1 true

let update_screen2= Screen.update screen2 4 4 100 1 1 true

let bottom_row = Queue.peek screen2

let rec block_checker l = 
  match l with 
  | [] -> 0
  | h :: t -> 
    let obj = extract_obj h in
    print_endline(Object.string_of_obj h);
    print_int(obj.width);
    print_endline("");
    1 + block_checker t

let screen_tests = [
  "Size of twice updated screen" >:: (fun _ ->
      assert_equal 
        1 (Queue.length screen2));

  "Size of updated screen of passable row" >:: (fun _ ->
      assert_equal
        1 (Queue.length screen2));

  "Size of first row of objects" >:: (fun _ ->
      assert
        (List.length bottom_row > 0)); 

  "Check random generation" >:: (fun _ ->
      assert
        (block_checker bottom_row > 0)); 
]

let suite =
  "test suite for Crossy Caml"  >::: List.flatten [
    actor_tests;
    object_tests;
    generator_tests;
    screen_tests;
  ]

let _ = run_test_tt_main suite