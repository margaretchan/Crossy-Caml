(** [generate x_bound y_bound num_pass grid_size] is a randomly generate 
    collidable list that has a block for every x < [x_bound] and for the 
    bottom row, [y_bound] - 4 block (determined by [grid_size]). 
    The bottom row has at least [num_pass] number of passable blocks. *)
val generate : int -> int -> int -> int -> Object.collidable list