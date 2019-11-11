open Graphics
open Draw
open Object
open Generator
open Screen

(** [display_loop last_update_time fps] controls the window refresh updates 
    [last_update_time] is the time the window was last redrawn 
                       not including refreshing from user input
    [fps] is the number of update frames per second (gauranteed minimum)
    The game is exited by closing the window *)
let display last_update_time fps = 
  let rec loop last_update_time fps player screen seq_bad_rows = 
    (* Check for time based update *)
    if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then (
      let dir = 
        if (key_pressed ()) then (
          let key = read_key () in
          if key = 'a' then -1 else (
            if key = 'd' then 1 else 0)) 
        else 0 in 
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

  let init_screen = Screen.empty in

  (* Initialize player at center of screen *)
  let init_player : collidable = Player {
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

let rec wait_for_start () = 
  match wait_next_event [Key_pressed] with 
  | status -> if read_key () = ' ' 
    then display (Sys.time ()) 30.0 
    else wait_for_start ()

(** Initializes game *)
let () = 
  Draw.init_window ();
  Draw.start_page ();
  wait_for_start ()