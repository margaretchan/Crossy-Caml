type dir = 
  | Down 
  | Left 
  | Right 
  | No

type pos = int * int

type effect = 
  | Adder
  | Multiplier of int
  | Phaser of int
  | Slower of int
  | Nothing

type block_type = 
  | SmallB 
  | LargeB 
  | GoodB of effect 

let get_effect b = 
  match b with 
  | GoodB e -> e
  | _ -> failwith "No effect"

let get_time b = 
  let eff = get_effect b in
  match eff with 
  | Multiplier i -> i 
  | Phaser i -> i
  | Slower i -> i
  | _ -> 0 

let is_good b = 
  match b with 
  | GoodB _ -> true
  | _ -> false