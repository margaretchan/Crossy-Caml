(** [generate_block coord grid_size typ dir spd] is a collidable object with 
    position [coord], type [typ], height and width = 2 * [grid_size], id = 
    id of last generated + 1, velocity of ([dir], [spd]) *)
val generate_block : Actor.pos -> int -> Actor.block_type -> Actor.dir -> int  
  -> Object.collidable

(** [generate x_bound y_pos num_pass grid_x grid_y] is a randomly generated 
    (collidable list * bool) that has a block for every x < [x_bound] and 
    for the bottom row, [y_pos] - 4 block (determined by [grid_x]). 
    The bool signals which direction the blocks in the row move (l/r).
    note: [y_pos] is the y coord of the bottom of the objects.
    [grid_y] is the number of pixels between every row of blocks
    The bottom row has at least [num_pass] number of passable blocks. *)
val generate : int -> int -> int -> int -> int -> Object.collidable list 