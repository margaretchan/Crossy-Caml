open Graphics
open Draw
open Object
open Screen
open State

(** [screen_fps] is the fps of the game screen *)
let screen_fps = 60.0

(** [down_fps] is the fps of the oject down movement *)
let down_fps = 2.0

(** [side_fps] is the fps of the oject side movement *)
let side_fps = 0.0

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

    let rec loop last_update_time fps player screen seq_good_rows 
        last_obj_down_time last_obj_side_time = 


      (** Check for Collisions, and if Lose, Set High Score & Draw Game Over *)
      if (Screen.collision_process player screen = Lose && 
          not (Object.has_phaser (Object.extract_obj player))) then 
        let () = Queue.clear screen in
        let obj = Object.extract_obj player in 
        if obj.score > high_score 
        then (
          Draw.game_over obj.score obj.score;
          display (Sys.time ()) screen_fps Lose obj.score
        )
        else (
          Draw.game_over obj.score high_score;
          display (Sys.time ()) screen_fps Lose high_score
        )
      else (

        (* Check for time based update *)
        if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then (
          let dir = 
            if (key_pressed ()) 
            then let key = read_key () in
              if key = 'a' 
              then -1 
              else (if key = 'd' 
                    then 1 
                    else 0)
            else 0 in 

          let obstacles_side =
            if ((Sys.time ()) -. last_obj_side_time) > (1.0 /. side_fps) && 
               not (Object.has_slower (Object.extract_obj player)) then 
              true else 
              false in

          let obstacles_down = 
            if ((Sys.time ()) -. last_obj_down_time > (1.0 /. down_fps)) 
            then          
              (** Update Score *)
              let obj = Object.extract_obj player in 
              Object.score_incr obj 1; 
              (**Update Effects List *)
              let obj = Object.extract_obj player in 
              obj.effects <- Object.update_effects obj.effects;
              true 
            else false in

          let updated_window = Draw.update_window dir player 
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
            loop (Sys.time ()) fps p s good last_obj_down_time' last_obj_side_time'
        ) 
        else (
          loop last_update_time fps player screen seq_good_rows 
            last_obj_down_time last_obj_side_time
        )
      ) in

    (** Initialize screen to Empty *)
    let init_screen = Screen.empty in

    (* Initialize player at center of screen *)
    let init_player : collidable =
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
      } in

    loop last_update_time fps init_player init_screen 3 (Sys.time ()) (Sys.time ())

  ) else 

    (** Lose State Logic *)
  if (st = Lose) then (

    let rec wait_for_reset () = 
      match wait_next_event [Key_pressed] with 
      | status -> 
        if read_key () = 'r' 
        then display (Sys.time ()) screen_fps Start high_score
        else wait_for_reset () in

    wait_for_reset();
  )

(** Initializes game *)
let () = 
  Draw.init_window ();
  display (Sys.time ()) screen_fps Start 0