open Graphics
open Draw

(** [display_loop last_update_time fps] controls the window refresh updates 
    [last_update_time] is the time the window was last refreshed 
    [fps] is the number of update frames per second 
    The game is exited by closing the window *)
let rec display_loop last_update_time fps = 
  if ((Sys.time ()) -. last_update_time > (1.0 /. fps)) 
  then ( 
    Draw.update_window ();
    display_loop (Sys.time ()) fps
  ) 
  else display_loop last_update_time fps

(** Initializes game *)
let () = 
  Draw.init_window ();
  display_loop (Sys.time ()) 30.0