open Actor
open Object
open Generator
open State

module Screen = struct

  open Queue

  let empty = create ()

  let obj_lst_from_tup tup = 
    match tup with 
    | lst, _ -> lst

  let collision_process player (s : (Object.collidable list * bool) Queue.t) =
    try (
      let bottom_list = obj_lst_from_tup (peek s) in
      (* filter the list to be only the blocks near the player *)
      let collide_with_player = Object.check_collision player in
      let collision_list = List.map collide_with_player bottom_list in
      let rec helper block_lst col_lst = 
        match block_lst, col_lst with
        | [], [] -> State.Game
        | h1 :: t1, h2 :: t2 -> if h2 then 
            if (Actor.is_good (Object.get_block h1))
            then State.Lose else State.Game
          else helper t1 t2
        | _ -> failwith "Oh no" in 
      helper bottom_list collision_list
    ) with
    | _ ->              
      State.Game 

  (** [shift_down col_lst] is a unit. It modifies all the collidables in 
      [col_lst] to have their position shifted down by their height. 
      Requires: [col_lst] contains no players *)
  let shift_down col_lst =
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | h :: t -> (match h with
          | Block (_, obj) -> obj.y_pos <- (obj.y_pos - obj.height)
          | Player _ -> failwith "List can't contain player");
        helper t in 
    helper (obj_lst_from_tup col_lst)

  (** [shift_side col_lst] is a unit. It modifies all the collidables in 
      [col_lst] to have their position shifted either left or right 
      (determined pseudo-randomly) by their width. 
      Requires: [col_lst] contains no players *)
  let shift_side col_lst = 
    let rec helper lst move_dir = 
      match lst with 
      | [] -> ()
      | b :: t -> begin
          match b with 
          | Block (_, obj) -> 
            obj.x_pos <- (obj.x_pos + (move_dir * obj.width)); 
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
    | h :: t, dir -> (match h with
        | Block (_, obj) -> obj
        | Player _ -> failwith "Should have no player in list")

  let update s x_bound y_bound num_pass grid_x grid_y = 
    (* shift down *)
    Queue.iter (shift_down) s;
    (* shift side *)
    Queue.iter (shift_side) s;
    (* generate new row on top *)
    let new_list = Generator.generate x_bound y_bound num_pass grid_x grid_y in
    Queue.push new_list s; 
    let bottom_row = Queue.peek s in
    let bottom_obj = get_obj bottom_row in
    let bottom = if bottom_obj.y_pos < 0 then Queue.pop s else Queue.peek s in 
    match bottom with 
    | _ -> s
end