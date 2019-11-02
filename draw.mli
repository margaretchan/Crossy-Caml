(** [init_window ()] opens the game window *)
val init_window : unit -> unit

(** [grid_x] is the number of pixels in one horizontal unit of the screen grid*)
val grid_x : int

(** [grid_y] is the number of pixels in one vertical unit of the screen grid *)
val grid_y : int

(** [update_window player_dir old_pos update_obstacles] is the 
    x position of the player avatar after drawing the updated game graphics 
    [player_dir] is the direction the player moves in:
    | -1 -> left
    | 0 -> no movement
    | 1 -> right 
    [old_pos] is the x position of the player avatar prior to update 
    [update_obstacles] is whether the obstacle positions should be updated *)
val update_window : int -> int -> bool -> int