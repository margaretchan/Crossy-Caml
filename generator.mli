(** [generate x_bound y_pos num_pass grid_x grid_y] is a randomly generated 
    collidable list that has a block for every x < [x_bound] and for the 
    bottom row, [y_pos] - 4 block (determined by [grid_x]). 
    note: [y_pos] is the y coord of the bottom of the objects.
    [grid_y] is the number of pixels between every row of blocks
    The bottom row has at least [num_pass] number of passable blocks. *)
val generate : int -> int -> int -> int -> int -> Object.collidable list