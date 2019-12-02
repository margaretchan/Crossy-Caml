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
  mutable effects : effect list
}

type collidable = 
  | Player of obj
  | Block of block_type * obj

let get_block c = 
  match c with 
  | Block (b,_) -> b
  | _ -> failwith "Not a block"

let check_collision (c1: collidable) (c2: collidable) : Actor.block_type option = 
  match c1, c2 with

  | Player ob1, Block (LargeB, ob2) when ( 
    (ob2.y_pos) < (ob1.y_pos + ob1.height) 
    && 
    ((ob2.x_pos < ob1.x_pos + ob1.width && ob2.x_pos > ob1.x_pos) 
     || (ob2.x_pos + ob2.width > ob1.x_pos && 
         ob2.x_pos + ob2.width < ob1.x_pos + ob1.width)
     || ob1.x_pos = ob2.x_pos && ob1.x_pos + ob1.width = ob2.x_pos + ob2.width)) 
    -> Some LargeB   

  | Player ob1, Block (SmallB, ob2) when (
    (ob2.y_pos) < (ob1.y_pos + ob1.height) 
    && 
    ((ob2.x_pos < ob1.x_pos + ob1.width && ob2.x_pos > ob1.x_pos) 
     || (ob2.x_pos + ob2.width > ob1.x_pos && 
         ob2.x_pos + ob2.width < ob1.x_pos + ob1.width)
     || ob1.x_pos = ob2.x_pos && ob1.x_pos + ob1.width = ob2.x_pos + ob2.width))
    -> Some SmallB

  | Player ob1, Block (GoodB effect, ob2) when (
    (ob2.y_pos) < (ob1.y_pos + ob1.height) 
    && 
    ((ob2.x_pos < ob1.x_pos + ob1.width && ob2.x_pos > ob1.x_pos) 
     || (ob2.x_pos + ob2.width > ob1.x_pos && 
         ob2.x_pos + ob2.width < ob1.x_pos + ob1.width)
     || ob1.x_pos = ob2.x_pos && ob1.x_pos + ob1.width = ob2.x_pos + ob2.width))
    -> Some (GoodB effect)
  | _ -> None 

let check_on_screen c xbound = 
  match c with
  | Block (_, ob) -> (ob.y_pos + ob.height) > 0
  | Player ob -> ob.y_pos > 0 
                 && (ob.x_pos + ob.width) > 0
                 && ob.y_pos < xbound    

(**[extract_obj c] extracts the Object from collidable [c] *)
let extract_obj (c : collidable) = 
  match c with 
  | Player obj -> obj 
  | Block (_, obj) -> obj

(** Decrements counters in [effs] and removes effects with timer zero *)
let update_effects (effs : effect list) : effect list =

  let eff_helper = function 
    | Adder i when i > 0 -> Adder (i - 1)
    | Multiplier i when i > 0 -> Multiplier (i - 1)
    | Phaser i when i > 0 -> Phaser (i - 1)
    | Slower i when i > 0 -> Slower (i - 1)
    | _ -> Nothing 
  in
  List.map eff_helper effs |> List.filter (fun eff -> eff != Nothing)

let is_phaser = function 
  | Phaser _ -> true 
  | _ -> false 

let has_phaser (player : obj) : bool = 
  List.exists is_phaser player.effects

let is_slower = function 
  | Slower _ -> true 
  | _ -> false 

let has_slower (player : obj) : bool = 
  List.exists is_slower player.effects

let is_mult = function 
  | Multiplier _ -> true 
  | _ -> false 

let has_mult (player : obj) : bool = 
  List.exists is_mult player.effects

let is_add = function 
  | Adder _ -> true 
  | _ -> false 

let has_adder (player : obj) : bool = 
  List.exists is_add player.effects

let score_incr p s = 
  if has_adder p then p.score <- p.score + 500;
  if has_mult p then 
    p.score <- p.score + 5*s
  else
    p.score <- p.score + s

let effect_time_left (effs : effect list) (e : effect) : int = 
  let effect_time_helper (acc : int) (eff: effect) : int = 
    match eff with 
    | Adder i when is_add e && i > acc -> i 
    | Multiplier i when is_mult e && i > acc -> i 
    | Phaser i when is_phaser e && i > acc -> i 
    | Slower i when is_slower e && i > acc -> i
    | _ -> acc in 
  List.fold_left (effect_time_helper) 0 (effs)





