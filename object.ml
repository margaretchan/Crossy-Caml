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

let score_incr p s = 
  (* if p has the multiplier effect active, change the score by 2 * s *)
  p.score <- p.score + s

let get_block c = 
  match c with 
  | Block (b,_) -> b
  | _ -> failwith "Not a block"

let check_collision c1 c2 = 
  match c1, c2 with
  | Player ob1, Block (LargeB, ob2) -> 
    (ob2.y_pos) < (ob1.y_pos + ob1.height) 
    && 
    ((ob2.x_pos < ob1.x_pos + ob1.width && ob2.x_pos > ob1.x_pos) 
     || (ob2.x_pos + ob2.width > ob1.x_pos && 
         ob2.x_pos + ob2.width < ob1.x_pos + ob1.width)
     || ob1.x_pos = ob2.x_pos && ob1.x_pos + ob1.width = ob2.x_pos + ob2.width)  

  | Player ob1, Block (SmallB, ob2) -> 
    (ob2.y_pos) < (ob1.y_pos + ob1.height) 
    && 
    ((ob2.x_pos < ob1.x_pos + ob1.width && ob2.x_pos > ob1.x_pos) 
     || (ob2.x_pos + ob2.width > ob1.x_pos && 
         ob2.x_pos + ob2.width < ob1.x_pos + ob1.width)
     || ob1.x_pos = ob2.x_pos && ob1.x_pos + ob1.width = ob2.x_pos + ob2.width)
  | _ -> false 

let check_on_screen c xbound = 
  match c with
  | Block (_, ob) -> (ob.y_pos + ob.height) > 0
  | Player ob -> ob.y_pos > 0 
                 && (ob.x_pos + ob.width) > 0
                 && ob.y_pos < xbound    