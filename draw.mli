open Graphics

(** [init_window ()] opens the game window *)
val init_window : unit -> unit

(** [background_color] is the color of the screen background *)
val background_color : Graphics.color

(** [player_color] is the color of the player on the screen *)
val player_color : Graphics.color

(** [update_window player_dir old_pos update_obstacles] is the 
    x position of the player avatar after drawing the updated game graphics 
    [player_dir] is the direction the player moves in:
    | -1 -> left
    | 0 -> no movement
    | 1 -> right 
    [old_pos] is the x position of the player avatar prior to update 
    [update_obstacles] is whether the obstacle positions should be updated *)
val update_window : int -> int -> bool -> int

(** Erases the current game window and draws the game over screen *)
val game_over : unit -> unit 
