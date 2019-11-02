open Graphics
open Draw

(** [display_loop last_update_time fps] controls the window refresh updates 
    [last_update_time] is the time the window was last redrawn 
                       not including refreshing from user input
    [fps] is the number of update frames per second (gauranteed minimum)
    The game is exited by closing the window *)
let display last_update_time fps = 
  let rec loop last_update_time fps curr_pos = 
    (* Check for time based update *)
    if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) then (
      let dir = 
        if (key_pressed ()) then (
          let key = read_key () in
          if key = 'a' then -1 else (
            if key = 'd' then 1 else 0)) 
        else 0 in 
      match Draw.update_window 0 curr_pos true with 
      | p -> loop (Sys.time ()) fps p
      (* also need to change /
         update_window parameters to know when its being updated by 
         time vs keyboard input. This will be important for moving obstacles *)
    )
    else (
      (* Check for user input based update *)
      if (key_pressed ()) then (
        let key = read_key () in
        let dir = if key = 'a' then -1 else (
            if key = 'd' then 1 else 0 ) in 
        match Draw.update_window dir curr_pos false with 
        | p -> loop last_update_time fps p
      )
      else loop last_update_time fps curr_pos
    ) in 
  (* Initialize player at center of screen *)
  loop last_update_time fps ((size_x ()) / 2)


(** Initializes game *)
let () = 
  Draw.init_window ();
  display (Sys.time ()) 30.0