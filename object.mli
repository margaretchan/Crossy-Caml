(** The type to represent an object *)
type obj = {
  mutable x_pos: int;
  mutable y_pos: int;
  velocity: Actor.dir * int;
  id: int;
  to_kill: bool;
  mutable score: int;
  height: int;
  width: int;
  mutable effects: Actor.effect list ; 
}

(** The type to represent a collidable object *)
type collidable = 
  | Player of obj 
  | Block of Actor.block_type * obj

(** [get_block c] is the block_type of collidable [c] 
    Requires: 
    - [c] is a collidable block
      Raises: 
      Failure "Not a block" if the collidable [c] is type player*)
val get_block : collidable -> Actor.block_type 

(** [check_collision c1 c2] is Some [c2] if collidable [c2] has overlapped 
    pixels with collidable [c1], None otherwise.
    Requires: 
    - [c1] is a collidable of Player
    - [c2] is a collidable of Block
      Raises: Failure "Failed requirement" if preconditions are unsatisfied*)
val check_collision : collidable -> collidable -> Actor.block_type option

(** [extract_obj c] extracts the obj from collidable [c] *)
val extract_obj : collidable -> obj

(** [update_effects effs] is an effect list with all counters in [effs] 
    decremented and removes effects with timer zero *)
val update_effects : Actor.effect list -> Actor.effect list 

(** [has_phaser player] is true iff the collidable's effect list contains a 
    phaser *)
val has_phaser : obj -> bool  

(** [has_slower player] is true iff the collidable's effect list contains a 
    slower *)
val has_slower : obj -> bool 

(** [has_mult player] is true iff the collidable's effect list contains a 
    multiplier *)
val has_mult : obj -> bool 

(** [has_adder player] is true iff the collidable's effect list contains an 
    adder *)
val has_adder : obj -> bool

(** [has_life player] is true iff the collidable's effect list contains a life*)
val has_life : obj -> bool

(** [has_clear player] is true iff the collidable's effect list contains a 
    clear *)
val has_clear : obj -> bool

(** [has_subtracter player] is true iff the collidable's effect list contains 
    a subtracter *)
val has_subtracter : obj -> bool

(** [has_speeder player] is true iff the collidable's effect list contains a 
    speeder *)
val has_speeder : obj -> bool

(** [score_incr p s] is a unit. It increments obj [p]'s score field by [s] *)
val score_incr : obj -> int -> unit

(** [effect_time_left effs e] is the maximum time of all effects of type 
    specified *)
val effect_time_left : Actor.effect list -> Actor.effect -> int 

(** [string_of_obj c] is the string representation of collidable c *)
val string_of_obj : collidable -> string


