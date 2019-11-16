open Actor
open Object
open Generator
open State

module Screen = struct

  open Queue

  let empty = create ()

  let collision_process player s =
    try (
      let bottom_list = peek s in
      (* filter the list to be only the blocks near the player *)
      let collide_with_player = Object.check_collision player in
      let collision_list = List.map collide_with_player bottom_list in
      let rec helper block_lst col_lst = 
        match block_lst, col_lst with
        | [], [] -> State.Game
        | h1 :: t1, h2 :: t2 -> if h2 then 
            if Actor.is_good (Object.get_block h1)
            then State.Game else State.Lose
          else helper t1 t2
        | _ -> failwith "Oh no" in 
      helper bottom_list collision_list
    ) with
    | _ ->              
      State.Game 

  (** [shift_down col_lst] is a unit. It modifies all the collidables in 
      [col_lst] to have their position shifted down by their height. 
      Requires: [col_lst] contains no players*)
  let shift_down (collidable_lst : collidable list) =
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | h :: t -> (match h with
          | Block (_, obj) -> obj.y_pos <- (obj.y_pos - obj.height)
          | Player _ -> failwith "List can't contain player");
        helper t in 
    helper collidable_lst

  (** [get_obj col_lst] is the object of the first colllidable of [col_lst]
      Requires: [col_lst] is not empty with no player collidables*)
  (* DOESN'T MATCH SPECIFICATION *)
  let get_obj collidable_lst = 
    match collidable_lst with
    | [] -> failwith "List is empty"
    | h :: t -> (match h with
        | Block (_, obj) -> obj
        | Player _ -> failwith "Should have no player in list")

  let update s x_bound y_bound num_pass grid_x grid_y = 
    (* shift down *)
    Queue.iter (shift_down) s; 
    if num_pass <= (30) 
    then let new_list = Generator.generate x_bound y_bound num_pass grid_x grid_y in
      Queue.push new_list s; 
    else ();
    let bottom_row = Queue.peek s in
    let bottom_obj = get_obj bottom_row in
    let bottom = if bottom_obj.y_pos < 0 then Queue.pop s else Queue.peek s in 
    match bottom with 
    | _ -> s
end