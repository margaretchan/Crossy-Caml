open Graphics

(** [init_window ()] opens the game window *)
val init_window : unit -> unit

(** [update_window player_dir old_pos update_obstacles] is the 
    x position of the player avatar after drawing the updated game graphics 
    [player_dir] is the direction the player moves in:
    | -1 -> left
    | 0 -> no movement
    | 1 -> right 
    [old_pos] is the x position of the player avatar prior to update 
    [update_obstacles] is whether the obstacle positions should be updated *)
val update_window : int -> int -> bool -> int

(** [start_page ()] draws the start screen of the game *)
val start_page : unit -> unit

(** [game_over ()] erases the current game window and draws the 
    game over screen *)
val game_over : unit -> unit 
