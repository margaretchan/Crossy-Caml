type dir = Down | Left | Right | No

type pos = int * int

type effect = 
  | Adder
  | Multiplier
  | Phaser
  | Slower
  | Nothing

type block_type = 
  | SmallB
  | LargeB
  | GoodB of effect

let get_effect = function
  | GoodB e -> e
  | _ -> failwith "No effect"