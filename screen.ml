open Actor
open Object
open Generator
open State

(* let extract_obj (c : collidable) = 
   match c with 
   | Player obj -> obj 
   | Block (_, obj) -> obj *)

module Screen = struct

  open Queue

  let empty = create ()

  type q = (Object.collidable list) Queue.t

  (** Returns State.Game if no collision with bad guy
      Returns State.Lose if collision with bad guy 
      Also updates player object with appropriate powerups
  *)
  let collision_process player (s : q) =
    try (
      let bottom_list = (peek s) in
      (* filter the list to be only the blocks near the player *)
      let collide_with_player = Object.check_collision player in
      let collision_list = List.filter_map collide_with_player bottom_list in
      if (List.exists (fun bt -> bt = LargeB || bt = SmallB) collision_list) then 
        State.Lose
      else 
        let eff_filter_helper = function 
          | GoodB eff -> true 
          | _ -> false in

        let extr_eff_helper = function 
          | GoodB eff -> eff 
          | _ -> failwith "This should never happen" in 

        let effect_list = List.map (extr_eff_helper) 
            (List.filter (eff_filter_helper) (collision_list)) in 

        let obj = Object.extract_obj player in 
        obj.effects <- obj.effects @ effect_list;
        State.Game
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
    helper (col_lst)

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
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | b :: t -> begin
          match b with 
          | Block (badtype, obj) -> 
            let move_dir = 
              let dir = fst obj.velocity in 
              if dir = Left 
              then -1 
              else (if dir = Right 
                    then 1 else 0) in
            let side_step_size = 
              if badtype = SmallB then 
                obj.width / 5 
              else if badtype = MediumB then 
                obj.width / 10 
              else obj.width / 15 in
            let normal = obj.x_pos + (move_dir * side_step_size) in
            let new_loc = loop_around_helper x_bound normal in
            obj.x_pos <- new_loc; 
            helper t
          | Player _ -> failwith "List can't contain player"
        end in 
    helper col_lst

  (** [get_obj col_lst] is the object of the first colllidable of [col_lst]
      Requires: [col_lst] is not empty with no player collidables *)
  let get_obj collidable_lst = 
    match collidable_lst with
    | [] -> failwith "List is empty"
    | h :: t -> begin 
        match h with
        | Block (_, obj) -> obj
        | Player _ -> failwith "Should have no player in list"
      end

  let update s x_bound y_bound num_pass grid_x grid_y toggle_down = 

    (* shift down *)
    if toggle_down then 
      Queue.iter (shift_down) s; 

    (* generate new row on top if bad *)
    if num_pass <= (30)
    then let new_list = Generator.generate x_bound y_bound num_pass grid_x grid_y in
      Queue.push new_list s; 
    else ();

    (* shift side *)
    Queue.iter (shift_side x_bound) s;

    if Queue.length s > 0 then 
      let bottom_row = Queue.peek s in
      if bottom_row = [] then let _ = (Queue.pop s) in s 
      else 
        let bottom_obj = get_obj bottom_row in
        let bottom = 
          if bottom_obj.y_pos < 0
          then Queue.pop s 
          else Queue.peek s in 
        match bottom with 
        | _ -> s
    else s
end