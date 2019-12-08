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

(** [generate_seed ()] is a unit. It initializes the random module with a seed
    that's dependent on the current time.  *)
let generate_seed () : unit = 
  let flt = Unix.time () in
  let seed_int = int_of_float flt in
  Random.init seed_int

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

(* FUNCTION IS NEVER USED *)
let check_on_screen c xbound = 
  match c with
  | Block (_, ob) -> (ob.y_pos + ob.height) > 0
  | Player ob -> ob.y_pos > 0 
                 && (ob.x_pos + ob.width) > 0
                 && ob.x_pos < xbound    

let extract_obj (c : collidable) = 
  match c with 
  | Player obj -> obj 
  | Block (_, obj) -> obj

let mystery_select () : Actor.effect = 
  generate_seed ();
  let rand_int = Random.int 8 in
  match rand_int with 
  | 0 -> Adder 0 
  | 1 -> Multiplier 10
  | 2 -> Phaser 10
  | 3 -> Slower 10
  | 4 -> Life 0
  | 5 -> Clear 0
  | 6 -> Speeder 20
  | 7 -> Subtracter 0
  | _ -> failwith "Random Int should not be > 7"

let update_effects (effs : effect list) : effect list =

  (* change adder helper to go to 0, maybe should make it go to Nothing *)
  let eff_helper = function 
    | Life i when i > 0 -> Life 0
    | Adder i when i > 0 -> Adder 0
    | Multiplier i when i > 0 -> Multiplier (i - 1)
    | Phaser i when i > 0 -> Phaser (i - 1)
    | Slower i when i > 0 -> Slower (i - 1)
    | Speeder i when i > 0 -> Speeder (i - 1)
    | Clear i when i > 0 -> Clear 0
    | Subtracter i when i > 0 -> Subtracter 0
    | Mystery i -> mystery_select ()
    | _ -> Nothing in
  List.map eff_helper effs |> List.filter (fun eff -> eff != Nothing)

(** TODO: DOC *)
let is_phaser = function 
  | Phaser _ -> true 
  | _ -> false 

let has_phaser (player : obj) : bool = 
  List.exists is_phaser player.effects

(** TODO: DOC *)
let is_slower = function 
  | Slower _ -> true 
  | _ -> false 

let has_slower (player : obj) : bool = 
  List.exists is_slower player.effects

(** TODO: DOC *)
let is_mult = function 
  | Multiplier _ -> true 
  | _ -> false 

let has_mult (player : obj) : bool = 
  List.exists is_mult player.effects

(** TODO: DOC *)
let is_add = function 
  | Adder _ -> true 
  | _ -> false 

(** TODO: DOC *)
let is_life = function 
  | Life _ -> true 
  | _ -> false

(** TODO: DOC *)
let is_clear = function 
  | Clear _ -> true 
  | _ -> false 

(** TODO: DOC *)
let is_subtracter = function 
  | Subtracter _ -> true 
  | _ -> false

(** TODO: DOC *)
let is_speeder = function 
  | Speeder _ -> true 
  | _ -> false

let is_mystery = function 
  | Mystery _ -> true 
  | _ -> false

let has_clear (player : obj) : bool = 
  List.exists is_clear player.effects

let has_subtracter (player : obj) : bool = 
  List.exists is_subtracter player.effects

let has_speeder (player : obj) : bool = 
  List.exists is_speeder player.effects

let has_adder (player : obj) : bool = 
  List.exists is_add player.effects

let has_life (player: obj) : bool = 
  List.exists is_life player.effects 

let score_incr p s = 
  if has_adder p then p.score <- p.score + 40;
  if has_subtracter p then p.score <- p.score - 40;
  if has_mult p then 
    p.score <- p.score + 5*s
  else
    p.score <- p.score + s

let effect_time_left (effs : effect list) (e : effect) : int = 
  let effect_time_helper (acc : int) (eff: effect) : int = 
    match eff with
    | Life i when is_life e && i > acc -> i 
    | Adder i when is_add e && i > acc -> i 
    | Multiplier i when is_mult e && i > acc -> i 
    | Phaser i when is_phaser e && i > acc -> i 
    | Slower i when is_slower e && i > acc -> i
    | Clear i when is_clear e && i > acc -> i
    | Speeder i when is_speeder e && i > acc -> i
    | Subtracter i when is_subtracter e && i > acc -> i
    | _ -> acc in 
  List.fold_left (effect_time_helper) 0 (effs)





