open Actor
open Object
open Generator
open State

module GameScreen = struct

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
        | h1 :: t1, h2 :: t2 -> if h2 then if (Object.get_block h1) <> GoodB 
            then State.Lose else State.Game
          else helper t1 t2
        | _ -> failwith "Oh no" in 
      helper bottom_list collision_list
    ) with
    | _ -> State.Game 

  let shift_down (collidable_lst : collidable list) =
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | h :: t -> (match h with
        | Block (_, obj) -> obj.y_pos <- (obj.y_pos - obj.height)
        | Player _ -> failwith "List can't contain player");
        helper t in 
    helper collidable_lst

  let check_y collidable_lst = 
    match collidable_lst with
    | [] -> failwith "List is empty"
    | h :: t -> (match h with
      | Block (_, obj) -> obj.y_pos
      | Player _ -> failwith "Should have no player in list")

  let update s b x_bound y_bound num_pass grid_x grid_y = 
    (* shift down *)
    Queue.iter (shift_down) s;
    (* shift_down s;  *)
    (* add top *)
    let new_list = Generator.generate x_bound y_bound num_pass grid_x grid_y in
    Queue.push new_list s; 
    (* check to remove bottom *)
    let bottom_row = Queue.peek s in
    let bottom_y = check_y bottom_row in
    if bottom_y < 0 then Queue.pop s else Queue.peek s
end