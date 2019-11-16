open Actor

type obj = {
  mutable x_pos: int;
  mutable y_pos: int;
  velocity: dir * int;
  id: int;
  to_kill: bool;
  mutable score: int;
  height: int;
  width: int;
}

type collidable = 
  | Player of obj
  | Block of block_type * obj

let get_x (x,_) = x

let get_y (_,y) = y

let get_block c = 
  match c with 
  | Block (b,_) -> b
  | _ -> failwith "Not a block"

let check_collision c1 c2 = 
  match c1, c2 with
  | Player ob1, Block (_, ob2) -> 
    (ob2.y_pos) < (ob1.y_pos + ob1.height) 
    && 
    (ob2.x_pos < (ob1.x_pos + ob1.width)
     || (ob2.x_pos + ob2.width) < (ob1.x_pos))
  | _, _ -> failwith "Failed requirement"

let check_on_screen c xbound = 
  match c with
  | Block (_, ob) -> (ob.y_pos + ob.height) > 0
  | Player ob -> ob.y_pos > 0 
                 && (ob.x_pos + ob.width) > 0
                 && ob.y_pos < xbound    