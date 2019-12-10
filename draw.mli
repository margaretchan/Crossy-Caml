(** Creates the actual display (game window) for the game. *)
open Graphics
open Screen

(** [init_window ()] opens the game window *)
val init_window : unit -> unit

(** [update_window last_player_dir player_dir player down_obstacles 
    side_obstacles screen seq_good_rows] is the tuple of:

    1. player object after drawing the updated game graphics,
    2. screen module representing the current objects on the screen, and
    3. number of sequential bad rows there has been.

    Its side effects are drawing and updating the game screen window *)
val update_window : int -> int -> Object.collidable -> bool -> bool 
  -> (Object.collidable list) Queue.t -> int -> int
  -> (Object.collidable * (Object.collidable list) Queue.t * int)

(** [start_page ()] is the game screen displayed at the start of the game *)
val start_page : unit -> unit

(** [pause ()] is the game screen displayed when the game screen is paused *)
val pause : unit -> unit

(** [continue lives score] is the game screen displayed when the player has died
    but there is a non-negative amount of lives left *)
val continue : int -> int -> unit

(** [game_over score high_score] is the game screen displayed when the player 
    has died and run out of lives *)
val game_over : int -> int -> unit 

(** [select ()] is the game screen displayed for selecting game difficulty *)
val select : unit -> unit 

