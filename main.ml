open Graphics
open Draw
open Object
open Screen
open State

let () = Draw.init_window () 

(** [screen_fps] is the fps of the game screen *)
let screen_fps = 60.0

(** [down_fps] is the fps of the oject down movement *)
let down_fps = 2.0

(** [side_fps] is the fps of the oject side movement *)
let side_fps = 2.0

let screen_ref = ref Screen.empty

(** [lives] is the number of lives the player has. Initalizes at 3. *)
let lives = ref 3 

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
  if (st = Start) then (
    let rec wait_for_start () = 
      match wait_next_event [Key_pressed] with 
      | status -> 
        if read_key () = ' ' 
        then display (Sys.time ()) screen_fps Game high_score
        else wait_for_start () in

    Draw.start_page ();
    wait_for_start();

  ) else

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
          display (Sys.time()) screen_fps Continue high_score
      else (
        (* Check for time based update *)
        if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then (

          let dir = ref 0 in 

          (** Get Player Input Direction  *)
          let () = 
            if (key_pressed ()) 
            then 
              let key = read_key () in
              if key = 'a' 
              then (dir := -1; 
                    last_player_dir := -1)
              else (if key = 'd' 
                    then (dir := 1;
                          last_player_dir := 1)
                    else (if key = 'p' 
                          then 
                            let () = dir := 0 in 
                            display (Sys.time ()) screen_fps Pause high_score;
                          else dir := 0 )) in 

          let obstacles_side =
            if ((Sys.time ()) -. last_obj_side_time) > (1.0 /. side_fps) && 
               not (Object.has_slower (Object.extract_obj player)) then 
              true else 
              false in

          let obstacles_down = 
            if ((Sys.time ()) -. last_obj_down_time > (1.0 /. down_fps)) 
            then          
              (* Update Score *)
              let obj = Object.extract_obj player in 
              Object.score_incr obj 1; 
              (* Update Effects List *)
              let obj = Object.extract_obj player in 
              obj.effects <- Object.update_effects obj.effects;
              true 
            else false in

          let updated_window = Draw.update_window !last_player_dir !dir player 
              obstacles_down obstacles_side screen seq_good_rows in 

          match updated_window with 
          | (p, s, good) -> 
            let last_obj_down_time' = 
              if obstacles_down 
              then (Sys.time ()) 
              else last_obj_down_time in 
            let last_obj_side_time' = 
              if obstacles_side 
              then (Sys.time ()) 
              else last_obj_side_time in 
            player_ref := p;
            screen_ref := s;
            loop (Sys.time ()) fps !player_ref !screen_ref good last_obj_down_time' last_obj_side_time' last_player_dir
        ) 
        else (
          loop last_update_time fps !player_ref !screen_ref seq_good_rows 
            last_obj_down_time last_obj_side_time last_player_dir
        )
      ) in

    loop last_update_time fps !player_ref !screen_ref 3 (Sys.time ()) (Sys.time ()) last_player_dir

  ) else 

    (** Lose State Logic *)
  if (st = Lose) then (
    player_ref := init_player;
    screen_ref := Screen.empty;
    let rec wait_for_reset () = 
      match wait_next_event [Key_pressed] with 
      | status -> 
        if read_key () = 'r' 
        then display (Sys.time ()) screen_fps Start high_score
        else wait_for_reset () in

    wait_for_reset();
  ) else 

  if (st = Pause) then (
    Draw.pause ();
    let rec wait_for_unpause () = 
      if key_pressed () && read_key () = 'p' then 
        display (Sys.time ()) screen_fps Game high_score
      else 
        wait_for_unpause () in
    wait_for_unpause ();
  ) else 

  if (st = Continue) then (
    Draw.continue !lives;
    let rec wait_for_continue () = 
      if key_pressed () && read_key () = 'f' then 
        display (Sys.time ()) screen_fps Game high_score
      else 
        wait_for_continue () in
    wait_for_continue ();
  )

(** Initializes game *)
let () =
  display (Sys.time ()) screen_fps Start 0