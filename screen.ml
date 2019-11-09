open Actor
open Object
open Generator
open State

module Screen = struct
  include Queue

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

  let shift_down (collidable_lst : obj list) =
    let rec helper lst = 
      match lst with 
      | [] -> ()
      | h :: t -> h.y_pos <- (h.y_pos - h.height);
        helper t in 
    helper collidable_lst

  let update s b = 
    (* shift down *)
    shift_down s
    (* add top *)
    (* check to remove bottom *)
end
