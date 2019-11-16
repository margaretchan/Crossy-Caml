module Screen : sig 

  (** [empty] is the empty screen *)
  val empty : (Object.collidable list * bool) Queue.t 

  (** [obj_lst_from_tup tup] is the list of collidable objects that represents 
      a row *)
  val obj_lst_from_tup : (Object.collidable list * bool) 
    -> Object.collidable list

  (** [collision_process p s] is the State of the game upon this update. 
      If [p] collides with a bad block from [s] then it is the Lose state. 
      Else it is the Game state. *)
  val collision_process : Object.collidable 
    -> (Object.collidable list * bool) Queue.t -> State.state

  (** [update s x y n gx gy] is the new [s] with all the collidables in [s] 
      shifted down and adds a new row. If the screen is full, then it no longer 
      stores the information of the bottom-most row.*)
  val update : (Object.collidable list * bool) Queue.t 
    -> int -> int -> int -> int -> int 
    -> (Object.collidable list * bool) Queue.t

end
