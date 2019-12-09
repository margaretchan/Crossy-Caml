type dir = 
  | Down 
  | Left 
  | Right 
  | No

type pos = int * int

type effect = 
  | Life of int
  | Adder of int
  | Multiplier of int
  | Phaser of int
  | Slower of int
  | Clear of int 
  | Speeder of int 
  | Subtractor of int 
  | Mystery of int
  | Nothing

type block_type = 
  | SmallB 
  | MediumB 
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

let string_of_eff = function
  | Life int -> "Life"
  | Adder int -> "Adder"
  | Multiplier int -> "Multiplier"
  | Phaser int -> "Phaser"
  | Slower int -> "Slower"
  | Clear int -> "Clear"
  | Speeder int -> "Speeder"
  | Subtractor int -> "Sub"
  | Mystery int -> "Mystery"
  | Nothing -> "Nothing"