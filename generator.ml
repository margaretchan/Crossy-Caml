open Actor
open Object

(** [generate_seed ()] is a unit. It initializes the random module with a seed
    that's dependent on the current time.  *)
let generate_seed () : unit = 
  let flt = Unix.time () in
  let seed_int = int_of_float flt in
  Random.init seed_int

(** [counter] is the number of objects generated *)
let counter = ref 0

(** [which_item ()] is an Actor.effect, excluding Nothing, chosen with equal
      probability. *)
let which_effect () : Actor.effect = 
  let () = generate_seed () in
  let item = Random.int 8 in
  match item with
  | 0 -> Adder 0 
  | 1 -> Multiplier 10
  | 2 -> Phaser 10
  | 3 -> Slower 10
  | 4 -> Life 0
  | 5 -> Clear 0
  | 6 -> Speeder 20
  | 7 -> Subtracter 0
  | _ -> failwith "Should never happen"

(** [generate_rand_item i] is an Actor.effect. It is Nothing with a 100 - i % 
    chance. With i% it will choose an effect, excluding Nothing. *)
let generate_rand_item i : Actor.effect = 
  let () = generate_seed () in
  let chance_of_item = Random.int 100 in
  if chance_of_item < i then 
    which_effect ()
  else 
    Nothing

(** [generate_rand_blk_type b_left p_left] is a tuple of 
    (Actor.block_type * block_length) with the block type chosen at random 
    given the constraint of blocks left and p_left to ensure there are enough
    passable blocks*)
let generate_rand_blk_type b_left p_left = 
  let () = generate_seed () in 
  (* what is the largest possible width of a generated block) *)
  let possible_not_pass = 
    if b_left - p_left > 3 
    then 3
    else b_left - p_left in
  let blk_type_rand = Random.int possible_not_pass in 
  let blk_type = 
    if blk_type_rand = 0 then 
      Actor.SmallB
    else 
      (if blk_type_rand = 1 
       then Actor.MediumB
       else Actor.LargeB) in
  blk_type, blk_type_rand + 1

(** [score_of_typ t s] is the score of the block type. If it's a GoodB Adder, 
    then the score is s, else 0. *)
let score_of_typ typ s: int = 
  match typ with 
  | GoodB e -> 
    begin 
      match e with 
      | Adder _ -> s
      | _ -> 0
    end
  | _ -> 0

let generate_block coord grid_size typ dir spd : collidable = 
  counter := !counter + 1;
  let width_mult = 
    match typ with 
    | SmallB -> 1
    | GoodB _ -> 1
    | MediumB -> 2
    | _ -> 3 in 
  match coord with
  | (x, y) ->
    Block (typ, {
        x_pos = x;
        y_pos = y;
        velocity = (dir, spd);
        id = !counter;
        to_kill = false;
        score = score_of_typ typ 40;
        height = 2 * grid_size;
        width = 2 * grid_size * width_mult; 
        effects = []
      })

(** [gen_helper coord x_bound y_bound cur_pass num_pass grid_size list] is a 
    (collidable list * bool) that has generated collidable objects filling up 
    all the grid spaces from x = 0 to x = [x_bound]. 
    The bool represents the direction the blocks in the row should move. *)
let rec gen_helper coord x_bound cur_pass num_pass grid_size list dir spd =
  match coord with
  | (x, y) -> 
    let total_width = x_bound - x in
    let block_width = 2 * grid_size in
    let blocks_left = total_width / block_width in 
    let pass_left = num_pass - cur_pass in
    if (blocks_left <= 0 || pass_left > blocks_left) 
    then list
    else 
      let rand = Random.int (x_bound / block_width) in 
      if (pass_left >= blocks_left || (rand < num_pass && pass_left > 0)) 
      then 
        let eff = generate_rand_item 10 in
        let pass_block = generate_block coord grid_size (GoodB eff) dir spd in
        gen_helper (x + block_width, y) x_bound (cur_pass + 1) num_pass 
          grid_size (pass_block :: list) dir spd
      else 
        let rand_blk_tuple = generate_rand_blk_type blocks_left pass_left in 
        let new_block = generate_block coord grid_size (fst rand_blk_tuple) dir spd in
        gen_helper (x + (snd rand_blk_tuple) * block_width, y) x_bound cur_pass 
          num_pass grid_size (new_block :: list) dir spd

let generate (x_bound : int) (y_bound : int) (num_pass : int) (grid_x : int) 
    (grid_y : int) : Object.collidable list = 

  let () = generate_seed () in

  let random_dir = 
    let chance_of_dir = Random.int 3 in
    match chance_of_dir with
    | 0 -> Left
    | 1 -> No
    | _ -> Right in

  let start_coord = (0, y_bound) in
  let rand_dir = random_dir in
  gen_helper start_coord x_bound 0 num_pass grid_x [] rand_dir 0