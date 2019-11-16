type dir =  Down | Left | Right | No

(** A tuple representing the x, y coordinates of an actor *)
type pos = int * int

(** The effect of an item block.*)
type effect = 
  | Adder
  | Multiplier
  | Phaser
  | Slower
  | Nothing

(** The type of obstacle block. *)
type block_type = 
  | SmallB
  | LargeB
  | GoodB of effect

(** [get_effect b] is the effect associated with block [b] *)
val get_effect : block_type -> effect

(** [is_good b] is true if block [b] is a GoodB *)
val is_good : block_type -> bool