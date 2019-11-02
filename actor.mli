type dir =  Down | Left | Right

(** A tuple representing the x, y coordinates of an actor *)
type pos = int * int

(* The type of obstacle block *)
type block_type = 
  | SmallB
  | LargeB
  | GoodB

