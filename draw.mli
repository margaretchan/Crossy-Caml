(** [init_window ()] opens the game window *)
val init_window : unit -> unit

(** [update_window player_dir old_pos ] is the x position of the player 
    avatar after drawing the updated game graphics 
    [player_dir] is the direction the player moves in:
    | -1 -> left
    | 0 -> no movement
    | 1 -> right 
    [old_pos] is the x position of the player avatar prior to update *)
val update_window : int -> int -> int