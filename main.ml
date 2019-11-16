open Graphics
open Draw
open Object
open Generator
open Screen
open State


(** Initializes state *)
let state = ref Start

(** Initialize High Score *)
let high_score = ref 0

(** Very janky implementation *)
let current_score = ref 0

(** Initialize fps *)
let set_fps = 3.0

(** [display_loop last_update_time fps] controls the window refresh updates 
    [last_update_time] is the time the window was last redrawn 
                       not including refreshing from user input
    [fps] is the number of times per second a new row is drawn on the screen
          (independant of keyboard update)
    The game is exited by closing the window *)
let rec display last_update_time fps = 

  (** Start State Logic *)
  if (!state = Start) then (

    let rec wait_for_start () = 
      match wait_next_event [Key_pressed] with 
      | status -> if read_key () = ' ' then 
          let () = (state := Game) in 
          display (Sys.time ()) set_fps
        else wait_for_start () in

    Draw.start_page ();
    wait_for_start();

  ) else 

    (** Game State Logic *)
  if (!state = Game) then (

    let rec loop last_update_time fps player screen seq_bad_rows = 

      (** Check for Collisions, and if Lose, Set High Score & Draw Game Over *)
      state := Screen.collision_process player screen;
      if (!state = Lose) then 
        let () = Queue.clear screen in
        let obj = Draw.extract_obj player in 
        if obj.score > !high_score then high_score := obj.score; 
        Draw.game_over obj.score !high_score;
        display (Sys.time ()) set_fps
      else

        (* Check for time based update *)
      if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then (
        let dir = 
          if (key_pressed ()) then (
            let key = read_key () in
            if key = 'a' then -1 else (
              if key = 'd' then 1 else 0)) 
          else 0 in 
        let obj = Draw.extract_obj player in 
        obj.score <- obj.score + 1;
        match (Draw.update_window dir player true screen seq_bad_rows) with 
        | (p, s, bad) -> loop (Sys.time ()) fps p s bad
      ) 
      else (
        (* Check for user input based update *)
        if (key_pressed ()) then (
          let key = read_key () in
          let dir = if key = 'a' then -1 else (
              if key = 'd' then 1 else 0 ) in 
          match (Draw.update_window dir player false screen seq_bad_rows) with 
          | (p, s, bad) -> loop last_update_time fps p s bad
        )
        else loop last_update_time fps player screen seq_bad_rows
      ) in 

    (** Initialize screen to Empty *)
    let init_screen = Screen.empty in

    (* Initialize player at center of screen *)
    let init_player : collidable =
      Player {
        x_pos = (size_x () / 2); 
        y_pos = (size_y () / 7);
        velocity = No, 0 ;
        id = 0;
        to_kill = false;
        score = 0;
        height = 2*(size_y () / 50); 
        width = 2*(size_x () / 30);
      } in

    loop last_update_time fps init_player init_screen 0

  ) else 

    (** Lose State Logic *)
  if (!state = Lose) then (

    let rec wait_for_reset () = 
      match wait_next_event [Key_pressed] with 
      | status -> if read_key () = 'r' then 
          let () = (state := Start) in 
          display (Sys.time ()) set_fps
        else wait_for_reset () in

    wait_for_reset();
  )

(** Initializes game *)
let () = 
  Draw.init_window ();
  display (Sys.time ()) set_fps
