open Actor
open Object

(** [generate_block coord grid_size typ] is a collidable object with position 
[coord], type [typ], height and width = 2 * [grid_size]. *)
let generate_block coord grid_size typ : collidable = 
  match coord with
  | (x, y) -> Block (typ, {
      position = (x, y);
      velocity = (No, 0);
      id = 0;
      to_kill = false;
      score = 0;
      height = 2 * grid_size;
      width = 2 * grid_size; 
  })
  | _ -> failwith "generate_pass given invalid coordinate"

(** [gen_helper coord x_bound y_bound cur_pass num_pass grid_size list] is a 
collidable list that has generated collidable objects filling up all the blocks
from x = 0 to x = [x_bound].*)
let rec gen_helper coord x_bound y_bound cur_pass num_pass grid_size list 
: collidable list =
  match coord with
  | (x, y) -> 
    if (x > x_bound) && (y > y_bound) then list 
    else if (cur_pass < num_pass) && (Random.int (x_bound / (2 * grid_size)) < 3)
      then let pass_block = generate_block coord grid_size GoodB in
      gen_helper (x+grid_size*2, y) x_bound y_bound (cur_pass + 1) num_pass grid_size (pass_block :: list)
      else let new_block = generate_block coord grid_size LargeB in
      gen_helper (x + grid_size*2, y) x_bound y_bound cur_pass num_pass grid_size (new_block :: list)
  | _ -> failwith "gen_helper given invalid coordinate"

let generate (x_bound:int) (y_bound:int) (num_pass:int) (grid_size:int) 
: collidable list = 
  let start_coord = (0, y_bound - 4*grid_size) in
  let first_row = gen_helper start_coord x_bound y_bound 0 num_pass grid_size [] in
  let second_coord = (0, y_bound - 2*grid_size) in
  gen_helper second_coord x_bound y_bound num_pass num_pass grid_size first_row
