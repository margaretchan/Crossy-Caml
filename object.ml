open Actor

type obj = {
  position: pos;
  velocity: dir * int;
  id: int;
  to_kill: bool;
  score: int;
  height: int;
  width: int;
}

type collidable = 
  | Player of obj
  | Block of block_type * obj

let get_x (x,_) = x

let get_y (_,y) = y

let check_collision c1 c2 = 
  match c1, c2 with
  | Player ob1, Block (_, ob2) -> 
      (get_y (ob2.position) <= get_y (ob1.position) + ob1.height 
   && get_x (ob2.position) <= (get_x (ob1.position)) + ob1.width)
   && (get_y (ob2.position) <= get_y (ob1.position) + ob1.height 
   && (get_x (ob2.position)) + ob2.width >= get_x (ob1.position))
  | _, _ -> failwith "Failed requirement"

let check_on_screen c xbound = 
  match c with
  | Block (_, ob) -> (get_y (ob.position)) + ob.height > 0
  | Player ob -> get_y (ob.position) > 0 
              && (get_x (ob.position)) + ob.width > 0
              && get_x (ob.position) < xbound    