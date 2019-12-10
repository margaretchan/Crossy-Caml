(** Creates blocks (obstacles and items) and rows of blocks *)

(** [generate_block coord grid_size typ dir spd] is a collidable block with 
    position [coord], type [typ], height and width = 2 * [grid_size], id = 
    id of last generated + 1, velocity of ([dir], [spd]) *)
val generate_block : Actor.pos -> int -> Actor.block_type -> Actor.dir -> int  
  -> Object.collidable

(** [generate x_bound y_pos num_pass grid_x grid_y] is a randomly generated 
    collidable list that has a block for every grid in 0 < [x_bound] with y_coord
    [y_pos]. The row has at least [num_pass] number of passable blocks and a 
    random direction. *)
val generate : int -> int -> int -> int -> Object.collidable list 