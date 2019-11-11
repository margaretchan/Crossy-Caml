module Screen : sig 

  val empty : Object.collidable list Queue.t 

  val collision_process : Object.collidable -> Object.collidable list Queue.t 
    -> State.state

  (* val update : Object.collidable list Queue.t -> bool 
     -> Object.collidable list Queue.t *)

end