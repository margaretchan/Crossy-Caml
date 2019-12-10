open Actor
open Object
open Generator
open State

module Screen = struct

  open Queue

  let empty = create ()

  (** The type of a screen *)
  type q = (Object.collidable list) Queue.t

  (** [effect_filter_helper effect] is whether the block [block] is a good block 
      with an associated effect *)
  let effect_filter_helper block = 
    match block with 
    | GoodB _ -> true
    | _ -> false

  let process_collision player (s : q) =
    try (
      let bottom_list = peek s in
      let collide_with_player = Object.check_collision player in
      let collision_list = List.filter_map collide_with_player bottom_list in
      if (List.exists (fun bt -> bt = LargeB || bt = SmallB) collision_list) 
      then State.Lose
      else 
        let effect_list = List.map (Actor.get_effect) 
            (List.filter (effect_filter_helper) (collision_list)) in 
        let obj = Object.extract_obj player in 
        obj.effects <- obj.effects @ effect_list;
        State.Game
    ) with
    | Queue.Empty -> State.Game 

  (** [shift_down col_lst] is a unit. It modifies all the collidables in 
      [col_lst] to have their position shifted down by their height. 
      Requires: [col_lst] contains no players 
      Raises: Failure "List can't contain player" *)
  let shift_down col_lst =
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | h :: t -> begin 
          match h with
          | Block (_, obj) -> obj.y_pos <- (obj.y_pos - obj.height)
          | Player _ -> failwith "List can't contain player" end;
        helper t in 
    helper (col_lst)

  (** [loop_around_helper x_bound normal] is the new x position the 
      bottom left corner of the block should be in after shifting to the side. 
      This also checks for loop around functionality. 
      [normal] is the position the block would've moved to if not accounting 
      for loop around. *)
  let loop_around_helper x_bound normal = 
    if normal < 0 
    then x_bound
    else 
      begin
        if normal >= x_bound
        then 0
        else normal
      end

  (** [get_side_step_size badtype obj] is the number of pixels [obj] should be 
      moved to the side. *)
  let get_side_step_size badtype obj = 
    if badtype = SmallB then 
      obj.width / 5 
    else if badtype = MediumB then 
      obj.width / 10 
    else obj.width / 15 

  (** [get_move_side_dir obj] is -1 = Left, 0 = None, or 1 = Right, representing
      the direction to the side obj should move. *)
  let get_move_side_dir obj = 
    let dir = fst obj.velocity in
    if dir = Left 
    then -1 
    else (if dir = Right 
          then 1 else 0)

  (** [shift_side x_bound col_lst] is a unit. It modifies all the collidables 
      in [col_lst] to have their position shifted either left or right 
      (determined pseudo-randomly) by their width. 
      [x_bound] is the screen bound in the x dimension.
      Requires: [col_lst] contains no players. *)
  let shift_side x_bound col_lst = 
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | b :: t -> begin
          match b with 
          | Block (badtype, obj) -> 
            let move_dir = get_move_side_dir obj in 
            let side_step_size = get_side_step_size badtype obj in
            let normal = obj.x_pos + (move_dir * side_step_size) in
            let new_loc = loop_around_helper x_bound normal in
            obj.x_pos <- new_loc; 
            helper t
          | Player _ -> failwith "List can't contain player"
        end in 
    helper col_lst

  (** [get_obj col_lst] is the object of the first colllidable of [col_lst].
      Requires: [col_lst] is not empty with no player collidables. *)
  let get_obj collidable_lst = 
    match collidable_lst with
    | [] -> failwith "List is empty"
    | h :: t -> begin 
        match h with
        | Block (_, obj) -> obj
        | Player _ -> failwith "Should have no player in list"
      end

  (** [remove_bottom_row screen] is the updated screen [screen] after the bottom
      row has been removed from the Queue. This row is only removed if its 
      position is off the screen. *)
  let remove_bottom_row screen = 
    let bottom_row = Queue.peek screen in
    if bottom_row = [] then 
      let _ = (Queue.pop screen) in 
      screen
    else 
      let bottom_obj = get_obj bottom_row in
      let bottom = 
        if bottom_obj.y_pos < 0
        then Queue.pop screen 
        else Queue.peek screen in 
      match bottom with 
      | _ -> screen

  let update screen x_bound y_bound num_pass grid_x grid_y toggle_down = 
    if toggle_down then 
      Queue.iter (shift_down) screen; 
    if num_pass <= (30) then 
      let new_list = 
        Generator.generate x_bound y_bound num_pass grid_x in
      Queue.push new_list screen; 
    else ();
    Queue.iter (shift_side x_bound) screen;
    if Queue.length screen > 0 then 
      remove_bottom_row screen
    else screen

end