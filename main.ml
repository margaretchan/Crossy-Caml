open Graphics
open Draw
open Object
open Screen
open State

(** This initializes the game window *)
let () = Draw.init_window () 

(** [screen_fps] is the fps of the game screen *)
let screen_fps = 60.0

(** [easy_down_fps] is the fps of objects moving down on easy mode *)
let easy_down_fps = 1.0 

(** [easy_side_fps] is the fps of objects moving to the side on easy mode *)
let easy_side_fps = 1.0

(** [normal_down_fps] is the fps of objects moving down on normal mode *)
let normal_down_fps = 2.0 

(** [normal_side_fps] is the fps of objects moving to the side on normal mode *)
let normal_side_fps = 4.0

(** [hard_down_fps] is the fps of objects moving down on hard mode *)
let hard_down_fps = 3.0 

(** [hard_side_fps] is the fps of objects moving to the side on hard mode *)
let hard_side_fps = 10.0

(** [speeder_down_fps] is the fps of objects moving down on while the Speeder 
    effect is applied *)
let speeder_down_fps = 4.0

(** [speeder_side_fps] is the fps of objects moving to the side while the 
    Speeder effect is applied *)
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

(** [screen_ref] is the reference to the Screen object throughout the game *)
let screen_ref = ref Screen.empty

(** [init_lives] is the number of lives the player begins the game with *)
let init_lives = 3

(** [lives] is the number of lives the player has *)
let lives = ref init_lives

(** [init_player] is the initial Player object at the start of the game *)
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

(** [player_ref] is the reference to the Player object throughout the game *)
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

(** [start_state_logic high_score] controls changes during Start State. 
    It draws start page then:
    If the user presses 's' then state changes to Select. 
    If the user presses 'Space' then state changes to Game. *)
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

  (** [assign_easy ()] changes difficulty, down_fps, and side_fps to 
      appropriate easy levels *)
and assign_easy () = 
  diff := Easy;
  down_fps := easy_down_fps;
  side_fps := easy_side_fps;


  (** [assign_easy ()] changes difficulty, down_fps, and side_fps to 
      appropriate normal levels *)
and assign_normal () = 
  diff := Normal;
  down_fps := normal_down_fps;
  side_fps := normal_side_fps;

  (** [assign_easy ()] changes difficulty, down_fps, and side_fps to 
      appropriate hard levels *)
and assign_hard () = 
  diff := Hard;
  down_fps := hard_down_fps;
  side_fps := hard_side_fps;

  (** [select_state_logic high_score] controls changes during Select State. 
      It draws the Select Page and changes difficulty based on user input. 
      If the user presses 'a', dififculty is changed to easy
      If the user presses 'b', dififculty is changed to normal 
      If the user presses 'c', dififculty is changed to hard *)
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

(** [continue_state_logic high_score] controls changes during Continue State.
    If the user presses 'f', the game returns to Game state  *)
and continue_state_logic high_score = 
  let rec wait_for_continue () = 
    if key_pressed () && read_key () = 'f' then 
      display (Sys.time ()) screen_fps Game high_score
    else 
      wait_for_continue () in
  wait_for_continue ()

(** [continue_state_logic high_score] controls changes during Pause State.
    It draws the Pause page and then if the user presses 'p', 
    the game returns to Game state  *)
and pause_state_logic high_score = 
  Draw.pause ();
  let rec wait_for_unpause () = 
    if key_pressed () && read_key () = 'p' then 
      display (Sys.time ()) screen_fps Game high_score
    else 
      wait_for_unpause () in
  wait_for_unpause ()

(** [assign_lose ()] reverts player_ref, screen_ref, and lives back to 
    their intiial values *)
and assign_lose () = 
  player_ref := init_player;
  screen_ref := Screen.empty;
  lives := init_lives;

  (** [lose_state_logic high_score] controls changes during Lose State.
      It reverts values back to their initial then if the user presses 'r', 
      the game returns to Start state  *)
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

(** [go_to_lose_logic obj s h] clears the screen and assigns appropriate 
    high score value. Then changes state to lose. *)
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

(** [go_to_continue_logic obj s h] clears the screen and assigns appropriate 
    life value. Then changes state to Continue. *)
and go_to_continue_logic obj screen high_score = 
  let () = (lives := !lives - 1) in 
  let () = Queue.clear screen in 
  Draw.continue !lives obj.score ;
  display (Sys.time()) screen_fps Continue high_score 

(** [collision_logic p s hs] controls collision logic after a collision has been 
    detected. If the player collides with an obstacle, change state to Lose or Continue
    depending on number of lives left *)
and collision_logic player screen high_score = 
  let obj = Object.extract_obj player in 
  if (!lives = 0) then 
    go_to_lose_logic obj screen high_score
  else 
    go_to_continue_logic obj screen high_score

(** [revert_speed_logic ()] returns down_fps and side_fps values back to 
    the appropriate values corresponding with the difficulty *)
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

    (** [item_effect_logic o s] controls logic for Clear, Life, and 
        Speeder items*)
and item_effect_logic obj screen = 
  if (Object.has_life obj) then lives := !lives + 1 ;
  if (Object.has_clear obj) then Queue.clear screen;
  if (Object.has_speeder obj) then (
    down_fps := speeder_down_fps; 
    side_fps := speeder_side_fps 
  );
  if (not (Object.has_speeder obj)) then 
    revert_speed_logic ()

(** [obstacle_down_logic lobt p s] updates score, does item effects, 
    and updates effect list when the condition specified by down_fps is met *)
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

(** [process_input_dir d lpd hs] controls input during game. 
    Sets direction to left when 'a' is pressed 
    Sets direction to right when 'd' is pressed
    Change state to Pause when 'p' is pressed  *)
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

(** [last_obj_down_time_logic od lodt] is last time down 
    obstacles have been updated *)
and last_obj_down_time_logic obstacles_down last_obj_down_time = 
  if obstacles_down then 
    (Sys.time ()) 
  else 
    last_obj_down_time

(** [last_obj_down_time_logic od lodt] is last time down 
    obstacles have been updated *)
and last_obj_side_time_logic obstacles_side last_obj_side_time = 
  if obstacles_side then 
    (Sys.time ()) 
  else 
    last_obj_side_time

(** [updated_window_logic uw] assigns player_ref and screen_ref based on 
    [updated_window]. Then returns # of good blocks/row. *)
and updated_window_logic updated_window = 
  match updated_window with 
  | (p, s, good) -> 
    player_ref := p;
    screen_ref := s;
    good

(** [obstacles_side_logic lost p] is true iff it is time for a new update 
    according to [lost] and side_fps *)
and obstacle_side_logic last_obj_side_time player = 
  ((Sys.time ()) -. last_obj_side_time) > (1.0 /. !side_fps) && 
  not (Object.has_slower (Object.extract_obj player)) 

(** [time_based_update_logic _] updates side and down obstacles when needed *)
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

(** [loop _] checks for collisions and sets state changes accordingly.
    It controls palyer input during game and moves obstacles on screen. *)
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

(** [game_state_logic _] controls all appropriate behavior during Game State *)
and game_state_logic high_score last_update_time fps = 
  let last_player_dir = ref 1 in
  loop last_update_time fps !player_ref !screen_ref 3 
    (Sys.time ()) (Sys.time ()) last_player_dir high_score

(** () initializies the game loop *)
let () =
  display (Sys.time ()) screen_fps Start 0