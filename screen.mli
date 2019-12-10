(** Stores, creates, and processes information of all the items on the game 
    screen.*)

(** [Screen] is the data structure that stores all the collidable objects
    being displayed to the user via the game window. *)
module Screen : sig 

  (** [empty] is the empty screen *)
  val empty : (Object.collidable list) Queue.t 

  (** [process_collision player s] is the State of the game upon this update. 
      If [player] collides with a bad/enemy block from [s] then it is the Lose 
      state. Else it is the Game state. 
      A side effect is that it updates the player object with the appropreate 
      power-ups upon a collision. *)
  val process_collision : Object.collidable 
    -> (Object.collidable list) Queue.t -> State.state

  (** [update screen x_bound y_bound num_pass grid_x grid_y toggle_down] is the 
      updated screen with all the collidables in [screen] shifted down according 
      to [toggle_down] and adds a new row. If the screen is full, then it no 
      longer stores the information of the bottom-most row. *)
  val update : (Object.collidable list) Queue.t 
    -> int -> int -> int -> int -> int -> bool
    -> (Object.collidable list) Queue.t

end
