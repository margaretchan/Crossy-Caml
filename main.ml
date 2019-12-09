open Graphics
open Draw
open Object
open Screen
open State

let () = Draw.init_window () 

(** [screen_fps] is the fps of the game screen *)
let screen_fps = 60.0

let easy_down_fps = 1.0 

let easy_side_fps = 1.0

let normal_down_fps = 2.0 

let normal_side_fps = 4.0

let hard_down_fps = 3.0 

let hard_side_fps = 10.0

let speeder_down_fps = 4.0

let speeder_side_fps = 15.0

(** [down_fps] is the fps of the oject down movement 
    Initializes at [normal_side_fps] *)
let down_fps = ref normal_down_fps

(** [side_fps] is the fps of the oject side movement. 
    Initializes at [normal_side_fps] *)
let side_fps = ref normal_side_fps

(** [diff] is hte difficulty of the Game.
    Initializes at [Normal] *)
let diff = ref Normal

let screen_ref = ref Screen.empty

let init_lives = 3

(** [lives] is the number of lives the player has *)
let lives = ref init_lives

let init_player =
  Player {
    x_pos = (size_x () / 2); 
    y_pos = (size_x () / 30);
    velocity = No, 0 ;
    id = 0;
    to_kill = false;
    score = 0;
    height = (size_x () / 30); 
    width = 2 * (size_x () / 30);
    effects = []
  }

let player_ref = ref init_player

(** [display last_update_time fps st high_score] controls the window 
    refresh updates 
    [last_update_time] is the time the window was last redrawn 
                       not including refreshing from user input
    [fps] is the number of times per second a new row is drawn on the screen
          (independant of keyboard update)
    [st] is the current Game State
    [High Score] is the current high score      
    The game is exited by closing the window *)
let rec display last_update_time fps st high_score = 
  if st = Start then start_state_logic high_score
  else if st = Game then game_state_logic high_score last_update_time fps
  else if st = Lose then lose_state_logic high_score
  else if st = Pause then pause_state_logic high_score
  else if st = Continue then continue_state_logic high_score
  else if st = Select then select_state_logic high_score

and start_state_logic high_score = 
  let rec wait_for_start () = 
    match wait_next_event [Key_pressed] with 
    | status -> 
      if read_key () = ' ' then 
        display (Sys.time ()) screen_fps Game high_score
      else if read_key () = 's' then 
        display (Sys.time ()) screen_fps Select high_score 
      else
        wait_for_start () in

  Draw.start_page ();
  wait_for_start();

and assign_easy () = 
  diff := Easy;
  down_fps := easy_down_fps;
  side_fps := easy_side_fps;

and assign_normal () = 
  diff := Normal;
  down_fps := normal_down_fps;
  side_fps := normal_side_fps;

and assign_hard () = 
  diff := Hard;
  down_fps := hard_down_fps;
  side_fps := hard_side_fps;

and select_state_logic high_score = 
  Draw.select ();
  let rec wait_for_select () = 
    if key_pressed () then 
      match read_key () with 
      | 'a' -> assign_easy ();
        display (Sys.time ()) screen_fps Start high_score
      | 'b' -> assign_normal ();
        display (Sys.time ()) screen_fps Start high_score
      | 'c' -> assign_hard ();
        display (Sys.time ()) screen_fps Start high_score
      | _ -> wait_for_select () 
    else 
      wait_for_select () in
  wait_for_select ()

and continue_state_logic high_score = 
  let rec wait_for_continue () = 
    if key_pressed () && read_key () = 'f' then 
      display (Sys.time ()) screen_fps Game high_score
    else 
      wait_for_continue () in
  wait_for_continue ()

and pause_state_logic high_score = 
  Draw.pause ();
  let rec wait_for_unpause () = 
    if key_pressed () && read_key () = 'p' then 
      display (Sys.time ()) screen_fps Game high_score
    else 
      wait_for_unpause () in
  wait_for_unpause ()

and assign_lose () = 
  player_ref := init_player;
  screen_ref := Screen.empty;
  lives := init_lives;

and lose_state_logic high_score = 
  assign_lose ();
  let rec wait_for_reset () = 
    match wait_next_event [Key_pressed] with 
    | status -> 
      if read_key () = 'r' then 
        display (Sys.time ()) screen_fps Start high_score
      else 
        wait_for_reset () in
  wait_for_reset()

and go_to_lose_logic obj screen high_score = 
  let () = Queue.clear screen in
  if obj.score > high_score then (
    Draw.game_over obj.score obj.score;
    let high_score = obj.score in 
    obj.score <- 0;
    display (Sys.time ()) screen_fps Lose high_score 
  ) 
  else (
    Draw.game_over obj.score high_score;
    obj.score <- 0;
    display (Sys.time ()) screen_fps Lose high_score
  )

and go_to_continue_logic obj screen high_score = 
  let () = (lives := !lives - 1) in 
  let () = Queue.clear screen in 
  Draw.continue !lives obj.score ;
  display (Sys.time()) screen_fps Continue high_score 

and collision_logic player screen high_score = 
  let obj = Object.extract_obj player in 
  if (!lives = 0) then 
    go_to_lose_logic obj screen high_score
  else 
    go_to_continue_logic obj screen high_score

and revert_speed_logic () = 
  match !diff with 
  | Easy ->
    down_fps := easy_down_fps;
    side_fps := easy_side_fps;
  | Normal -> 
    down_fps := normal_down_fps;
    side_fps := normal_side_fps;
  | Hard -> 
    down_fps := hard_down_fps;
    side_fps := hard_side_fps;

and item_effect_logic obj screen = 
  if (Object.has_life obj) then lives := !lives + 1 ;
  if (Object.has_clear obj) then Queue.clear screen;
  if (Object.has_speeder obj) then (
    down_fps := speeder_down_fps; 
    side_fps := speeder_side_fps 
  );
  if (not (Object.has_speeder obj)) then 
    revert_speed_logic ()

and obstacle_down_logic last_obj_down_time player screen = 
  if ((Sys.time ()) -. last_obj_down_time > (1.0 /. !down_fps)) then 
    (* Update Score *)
    let obj = Object.extract_obj player in 
    Object.score_incr obj 1; 
    (** Do Item Effects *)
    item_effect_logic obj screen;
    (* Update Effects List *)
    let obj = Object.extract_obj player in 
    obj.effects <- Object.update_effects obj.effects;
    true 
  else 
    false 

and process_input_dir dir last_player_dir high_score = 
  if (key_pressed ()) then 
    let key = read_key () in
    match key with 
    | 'a' -> 
      dir := -1;
      last_player_dir := -1
    | 'd' -> dir := 1;
      last_player_dir := 1
    | 'p' -> dir := 0;
      display (Sys.time ()) screen_fps Pause high_score;
    | _ -> dir := 0

and last_obj_down_time_logic obstacles_down last_obj_down_time = 
  if obstacles_down then 
    (Sys.time ()) 
  else 
    last_obj_down_time

and last_obj_side_time_logic obstacles_side last_obj_side_time = 
  if obstacles_side then 
    (Sys.time ()) 
  else 
    last_obj_side_time

and updated_window_logic updated_window = 
  match updated_window with 
  | (p, s, good) -> 
    player_ref := p;
    screen_ref := s;
    good

and obstacle_side_logic last_obj_side_time player = 
  ((Sys.time ()) -. last_obj_side_time) > (1.0 /. !side_fps) && 
  not (Object.has_slower (Object.extract_obj player)) 

and time_based_update_logic last_player_dir high_score last_obj_side_time 
    player screen seq_good_rows last_obj_down_time fps dir =
  let obstacles_side = obstacle_side_logic last_obj_side_time player in
  let obstacles_down = 
    obstacle_down_logic last_obj_down_time player screen in
  let updated_window = Draw.update_window !last_player_dir !dir player 
      obstacles_down obstacles_side screen seq_good_rows !lives in 
  let good = updated_window_logic updated_window in
  let last_obj_down_time' = 
    last_obj_down_time_logic obstacles_down last_obj_down_time in 
  let last_obj_side_time' = 
    last_obj_side_time_logic obstacles_side last_obj_side_time in
  loop (Sys.time ()) fps !player_ref !screen_ref good 
    last_obj_down_time' last_obj_side_time' last_player_dir high_score

and loop last_update_time fps player screen seq_good_rows 
    last_obj_down_time last_obj_side_time last_player_dir high_score = 

  (** Check for Collisions, and if Lose, Set High Score & Draw Game Over *)
  if (Screen.process_collision player screen = Lose && 
      not (Object.has_phaser (Object.extract_obj player))) then 
    collision_logic player screen high_score
  else if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then
    let dir = ref 0 in
    process_input_dir dir last_player_dir high_score; 
    time_based_update_logic last_player_dir high_score last_obj_side_time 
      player screen seq_good_rows last_obj_down_time fps dir
  else 
    loop last_update_time fps !player_ref !screen_ref seq_good_rows 
      last_obj_down_time last_obj_side_time last_player_dir high_score

and game_state_logic high_score last_update_time fps = 
  let last_player_dir = ref 1 in
  loop last_update_time fps !player_ref !screen_ref 3 
    (Sys.time ()) (Sys.time ()) last_player_dir high_score

(** Initializes game *)
let () =
  display (Sys.time ()) screen_fps Start 0