open Graphics
open Screen

(** [init_window ()] opens the game window *)
val init_window : unit -> unit

(** [update_window player_dir old_pos update_obstacles screen seq_bad_rows] 
    is the tuple of:
    1. player object after drawing the updated game graphics,
    2. screen module representing the current objects on the screen, and
    3. number of sequential bad rows there has been.
    [player_dir] is the direction the player moves in:
    | -1 -> left
    | 0 -> no movement
    | 1 -> right 
    [old_pos] is the x position of the player avatar prior to update 
    [update_obstacles] is whether the obstacle positions should be updated 
    [screen] is the current screen of the game
    [seq_bad_rows] is the number of sequential bad rows there has been *)
val update_window : int -> int -> Object.collidable -> bool -> bool 
  -> (Object.collidable list) Queue.t -> int -> int
  -> (Object.collidable * (Object.collidable list) Queue.t * int)

(** [start_page ()] draws the start screen of the game *)
val start_page : unit -> unit

(** [pause ()] draws the pause screen of the game *)
val pause : unit -> unit

(** [continue ()] draws the continue screen of the game *)
val continue : int -> unit

(** [game_over ()] erases the current game window and draws the 
    game over screen *)
val game_over : int -> int -> unit 

(** [select ()] draws the select screen of the game *)
val select : unit -> unit 

