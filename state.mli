(** The state of the game *)
type state = 
  | Start 
  | Game 
  | Lose 
  | Pause 
  | Continue
  | Select

(** The difficulty of the game *)
type difficulty = 
  | Easy 
  | Normal
  | Hard
