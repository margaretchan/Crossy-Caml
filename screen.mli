module Screen : sig 

  (** [empty] is the empty screen *)
  val empty : (Object.collidable list) Queue.t 

  (** [collision_process p s] is the State of the game upon this update. 
      If [p] collides with a bad block from [s] then it is the Lose state. 
      Else it is the Game state. *)
  val collision_process : Object.collidable 
    -> (Object.collidable list) Queue.t -> State.state

  (** [update s x y n gx gy] is the new [s] with all the collidables in [s] 
      shifted down and adds a new row. If the screen is full, then it no longer 
      stores the information of the bottom-most row.*)
  val update : (Object.collidable list) Queue.t 
    -> int -> int -> int -> int -> int 
    -> (Object.collidable list) Queue.t

end
