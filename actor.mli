(** Define basic types used in Object  *)

(** The type for the movement direction of a block *)
type dir =  
  | Down 
  | Left 
  | Right 
  | No

(** A tuple representing the x, y coordinates of an actor *)
type pos = int * int

(** The effect of a good block.*)
type effect = 
  | Life of int
  | Adder of int
  | Multiplier of int
  | Phaser of int
  | Slower of int 
  | Clear of int 
  | Speeder of int 
  | Subtractor of int
  | Mystery of int
  | Nothing

(** The type of blocks *)
type block_type = 
  | SmallB
  | MediumB
  | LargeB
  | GoodB of effect

(** [get_effect b] is the effect associated with block [b].
    Raises: 
      Failure "No effect" if the function is applied to anything but a 
      good block. *)
val get_effect : block_type -> effect

(** [get_time b] is the time associated with block [b] *)
val get_time : block_type -> int

(** [is_good b] is true if block [b] is a GoodB and false otherwise*)
val is_good : block_type -> bool

(** [string_of_eff e] is the string representation of effect e *)
val string_of_eff : effect -> string