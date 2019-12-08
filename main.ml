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

  (** Start State Logic *)
  if (st = Start) then 
    start_state_logic high_score
  else

    (** Game State Logic *)
  if (st = Game) then (

    let last_player_dir = ref 1 in

    let rec loop last_update_time fps player screen seq_good_rows 
        last_obj_down_time last_obj_side_time last_player_dir = 

      (** Check for Collisions, and if Lose, Set High Score & Draw Game Over *)
      if (Screen.collision_process player screen = Lose && 
          not (Object.has_phaser (Object.extract_obj player))) then 
        let obj = Object.extract_obj player in 
        if (!lives = 0) then 
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
        else 
          let () = (lives := !lives - 1) in 
          let () = Queue.clear screen in 
          Draw.continue !lives obj.score ;
          display (Sys.time()) screen_fps Continue high_score
      else (
        (* Check for time based update *)
        if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then (

          let dir = ref 0 in 

          (** Get Player Input Direction  *)
          let () = 
            if (key_pressed ()) then 
              let key = read_key () in
              if key = 'a' then 
                (dir := -1; 
                 last_player_dir := -1)
              else (if key = 'd' then 
                      (dir := 1;
                       last_player_dir := 1)
                    else (if key = 'p' then 
                            let () = dir := 0 in 
                            display (Sys.time ()) screen_fps Pause high_score;
                          else 
                            dir := 0 )) in 


          let obstacles_side =
            ((Sys.time ()) -. last_obj_side_time) > (1.0 /. !side_fps) && 
            not (Object.has_slower (Object.extract_obj player))  in

          let obstacles_down = 
            if ((Sys.time ()) -. last_obj_down_time > (1.0 /. !down_fps)) then 

              (* Update Score *)
              let obj = Object.extract_obj player in 
              Object.score_incr obj 1; 

              (** Do Item Effects *)
              if (Object.has_life obj) then 
                lives := !lives + 1 
              else ();

              if (Object.has_clear obj) then 
                Queue.clear screen
              else ();

              if (Object.has_speeder obj) then (
                down_fps := speeder_down_fps; 
                side_fps := speeder_side_fps 
              ) else ();

              if (not (Object.has_speeder obj)) then 
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
              else ();

              (* Update Effects List *)
              let obj = Object.extract_obj player in 
              obj.effects <- Object.update_effects obj.effects;
              true 
            else 
              false in

          let updated_window = Draw.update_window !last_player_dir !dir player 
              obstacles_down obstacles_side screen seq_good_rows !lives in 

          match updated_window with 
          | (p, s, good) -> 
            let last_obj_down_time' = 
              if obstacles_down then 
                (Sys.time ()) 
              else 
                last_obj_down_time in 
            let last_obj_side_time' = 
              if obstacles_side then 
                (Sys.time ()) 
              else 
                last_obj_side_time in 

            player_ref := p;
            screen_ref := s;
            loop (Sys.time ()) fps !player_ref !screen_ref good 
              last_obj_down_time' last_obj_side_time' last_player_dir
        ) 
        else (
          loop last_update_time fps !player_ref !screen_ref seq_good_rows 
            last_obj_down_time last_obj_side_time last_player_dir
        )
      ) in

    loop last_update_time fps !player_ref !screen_ref 3 
      (Sys.time ()) (Sys.time ()) last_player_dir

  ) else 

    (** Lose State Logic *)
  if (st = Lose) then 
    lose_state_logic high_score
  else 

  if (st = Pause) then 
    pause_state_logic high_score
  else 

  if (st = Continue) then 
    continue_state_logic high_score
  else 

  if (st = Select) then 
    select_state_logic high_score

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


(** Initializes game *)
let () =
  display (Sys.time ()) screen_fps Start 0