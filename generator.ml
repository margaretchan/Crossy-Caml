open Actor
open Object

(** [generate_seed] is a unit. It initializes the random module with a seed
    that's dependent on the current time.  *)
let generate_seed : unit = 
  let flt = Unix.time () in
  let seed_int = int_of_float flt in
  Random.init seed_int

let counter = ref 0

let x = generate_seed

(** [which_item] is an Actor.effect, excluding Nothing, chosen with equal
      probability. *)
let which_effect : Actor.effect = 
  let item = Random.int 4 in
  match item with
  | 0 -> Adder
  | 1 -> Multiplier
  | 2 -> Phaser
  | 3 -> Slower
  | _ -> failwith "Should never happen"

(** [generate_rand_item i] is an Actor.effect. It is Nothing with a 100 - i % 
    chance. With i% it will choose an effect, excluding Nothing. *)
let generate_rand_item i : Actor.effect = 
  let chance_of_item = Random.int 100 in
  if chance_of_item < i
  then which_effect
  else Nothing

(** [score_of_typ t s] is the score of the block type. If it's a GoodB Adder, 
    then the score is s, else 0. *)
let score_of_typ typ s: int = 
  match typ with 
  | GoodB e -> 
    begin 
      match e with 
      | Adder -> 10000
      | _ -> 0
    end
  | _ -> 0

let random_dir = 
  let chance_of_dir = Random.int 3 in
  match chance_of_dir with
  | 0 -> Left
  | 1 -> No
  | _ -> Right

(** [generate_block coord grid_size typ i dir spd] is a collidable object with 
    position [coord], type [typ], height and width = 2 * [grid_size], id = 
    id of last generated + 1, velocity of ([dir], [spd]) *)
let generate_block coord grid_size typ dir spd : collidable = 
  counter := !counter + 1;
  match coord with
  | (x, y) ->
    Block (typ, {
        x_pos = x;
        y_pos = y;
        velocity = (dir, spd);
        id = !counter;
        to_kill = false;
        score = score_of_typ typ 5000;
        height = 2 * grid_size;
        width = 2 * grid_size; 
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
<<<<<<< HEAD
    if (blocks_left <= 0) 
    then list
    else let rand = Random.int (x_bound / block_width) in 
=======

    if (blocks_left <= 0) 
    then list
    else let rand = Random.int (x_bound / block_width) in 

>>>>>>> 5caf7a0dea96ccf7460024463d34e2db11519168
      if (pass_left = blocks_left || (rand < num_pass && pass_left > 0)) 
      then let eff = generate_rand_item 3 in
        let pass_block = generate_block coord grid_size (GoodB eff) dir spd in
        gen_helper (x + block_width, y) x_bound (cur_pass + 1) num_pass 
          grid_size (pass_block :: list) dir spd
      else let new_block = generate_block coord grid_size LargeB dir spd in
        gen_helper (x + block_width, y) x_bound cur_pass num_pass grid_size 
          (new_block :: list) dir spd

let generate (x_bound : int) (y_bound : int) (num_pass : int) (grid_x : int) 
    (grid_y : int) : Object.collidable list = 
  let start_coord = (0, y_bound) in
  let rand_dir = random_dir in
  gen_helper start_coord x_bound 0 num_pass grid_x [] rand_dir 0
