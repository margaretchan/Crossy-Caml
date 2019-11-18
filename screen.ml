open Actor
open Object
open Generator
open State

module Screen = struct

  open Queue

  let empty = create ()

  type q = (Object.collidable list * bool) Queue.t

  let obj_lst_from_tup tup = 
    match tup with 
    | lst, _ -> lst

  let collision_process player (s : q) =
    try (
      let bottom_list = obj_lst_from_tup (peek s) in
      (* filter the list to be only the blocks near the player *)
      let collide_with_player = Object.check_collision player in
      let collision_list = List.filter collide_with_player bottom_list in
      if collision_list = [] 
      then State.Game 
      else State.Lose
    ) with
    | Queue.Empty -> State.Game 

  (** [shift_down col_lst] is a unit. It modifies all the collidables in 
      [col_lst] to have their position shifted down by their height. 
      Requires: [col_lst] contains no players *)
  let shift_down col_lst =
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | h :: t -> begin 
          match h with
          | Block (_, obj) -> obj.y_pos <- (obj.y_pos - obj.height)
          | Player _ -> failwith "List can't contain player" end;
        helper t in 
    helper (obj_lst_from_tup col_lst)

  (** [loop_around_helper x_bound normal] is the new position the 
      bottom left corner of the block should be in after shifting to the side. 
      This also checks for loop around functionality. 
      [normal] is the position the block would've moved to if not accounting 
      for loop around  *)
  let loop_around_helper x_bound normal = 
    if normal < 0 
    then x_bound
    else (
      if normal >= x_bound
      then 0
      else normal
    )
  (** [shift_side x_bound col_lst] is a unit. It modifies all the collidables 
      in [col_lst] to have their position shifted either left or right 
      (determined pseudo-randomly) by their width. 
      [x_bound] is the screen bound in the x dimension.
      Requires: [col_lst] contains no players *)
  let shift_side x_bound col_lst = 
    let rec helper lst move_dir = 
      match lst with 
      | [] -> ()
      | b :: t -> begin
          match b with 
          | Block (_, obj) -> 
            let normal = obj.x_pos + (move_dir * obj.width) in
            let new_loc = loop_around_helper x_bound normal in
            obj.x_pos <- new_loc; 
            helper t move_dir
          | Player _ -> failwith "List can't contain player"
        end in 
    match col_lst with 
    | lst, dir -> 
      let move_dir = if dir then 1 else -1 in 
      helper lst move_dir

  (** [get_obj col_lst] is the object of the first colllidable of [col_lst]
      Requires: [col_lst] is not empty with no player collidables *)
  let get_obj collidable_lst = 
    match collidable_lst with
    | [], _ -> failwith "List is empty"
    | h :: t, dir -> begin 
        match h with
        | Block (_, obj) -> obj
        | Player _ -> failwith "Should have no player in list"
      end

  let update s x_bound y_bound num_pass grid_x grid_y = 
    (* shift down *)
    Queue.iter (shift_down) s; 
    if num_pass <= (30) 
    then let new_list = Generator.generate x_bound y_bound num_pass grid_x grid_y in
      Queue.push new_list s; 
    else ();
    (* shift side *)
    Queue.iter (shift_side x_bound) s;
    (* generate new row on top *)
    let new_list = Generator.generate x_bound y_bound num_pass grid_x grid_y in
    Queue.push new_list s; 
    let bottom_row = Queue.peek s in
    let bottom_obj = get_obj bottom_row in
    let bottom = if bottom_obj.y_pos < 0 then Queue.pop s else Queue.peek s in 
    match bottom with 
    | _ -> s
end