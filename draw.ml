open Graphics
open Actor
open Object
open Generator
open Screen

let init_window () = 
  open_graph " 750x750";
  set_window_title "Crossy Caml"

(** [start_page_color] is the background color of the start screen *)
let start_page_color = rgb 113 182 77

(** [text_color] is the color of the text on the start screen *)
let text_color = rgb 128 0 62

(** [background_color] is the color of the screen background *)
let background_color = rgb 255 207 57

(** [player_color] is the color of the player on the screen *)
let player_color = rgb 209 105 154

(** [bad_blk_color] is the color of the bad blocks on the screen *)
let bad_blk_color = rgb 23 97 62

(** [good_blk_color] is the color of the passable blocks on the screen *)
let good_blk_color = background_color

(** [draw_collidable collide] draws the collidable object [collide] on the 
    screen as a rectangle with its fields width and height.
    The color of the rectangle is dependant whether [collide] is a 
    good or bad block type *)
let draw_collidable collide = 
  match collide with 
  | Block (goodbad_type, obj) -> 
    set_color (if Actor.is_good goodbad_type then good_blk_color else bad_blk_color);
    fill_rect (obj.x_pos) (obj.y_pos) (obj.width) (obj.height);
  | Player obj -> 
    set_color player_color;
    fill_rect (obj.x_pos) (obj.y_pos) (obj.width) (obj.height) 

(**[extract_obj c] extracts the Object from collidable [c] *)
let extract_obj (c : collidable) = 
  match c with 
  | Player obj -> obj 
  | Block (_, obj) -> obj

(** [moved_player dir step x_bound player] is [player] but 
    moved one [step] in [dir] on a screen with x limit [x_bound]
    Generalize to move_collidable later *)
let moves_player (dir : int) (step : int) (x_bound : int) (player : collidable) 
  : unit  =
  let obj = extract_obj player in 
  (** Loop player position around screen *)
  let pos_after_step = (obj.x_pos) + (dir * step) in
  let new_x_pos = if pos_after_step > x_bound then 0 else
      (if (pos_after_step + step) < 0 then (x_bound - step) 
       else pos_after_step) in
  obj.x_pos <- new_x_pos

(** [get pos c] is the position of [c] *)
let get_pos (c : collidable) =  
  match c with 
  | Player obj -> (obj.x_pos, obj.y_pos)
  | Block (_, obj) -> (obj.x_pos, obj.y_pos)

let draw_row collidable_lst =
  let rec helper lst = 
    match lst with
    | [] -> ()
    | h :: t -> draw_collidable h; 
      helper t in 
  helper (Screen.obj_lst_from_tup collidable_lst)

let update_window player_dir (player : collidable) update_obstacles (screen : (Object.collidable list * bool) Queue.t) 
    seq_good_rows = 
  (* [grid_x] is the number of pixels in one horizontal unit of the 
       screen grid *)
  let grid_x = (size_x ()) / 30 in

  (* [grid_y] is the number of pixels in one vertical unit of the screen grid *)
  let grid_y = (size_y ()) / 50 in

  (* Fill background color:
     There is currently no way to set foreground and background colors?? 
     Filling a rectangle is so janky *)
  set_color background_color;
  fill_rect 0 0 (size_x ()) (size_y ());

  (* Update screen *)
  let screen' = 
    if update_obstacles then (
      let next_row_good = if seq_good_rows < 3 then true else false in
      let num_good_blks = if next_row_good then 30 else 5 in
      Screen.update screen (size_x ()) (size_y ()) num_good_blks grid_x grid_y)
    else screen in

  (* Draw blocks *)
  Queue.iter draw_row screen';

  (* Update number of sequential good rows *)
  let seq_good_rows' = if update_obstacles 
    then (if (seq_good_rows > 3) then 0 else seq_good_rows + 1) 
    else seq_good_rows in

  (* Update and Draw Player Collidable *)
  set_color player_color;
  moves_player player_dir grid_x (size_x ()) player;
  draw_collidable player;  

  (* Update Score *)
  let p_obj = extract_obj player in 
  set_color text_color;
  moveto 50 50;
  draw_string ("Score: " ^ (string_of_int p_obj.score));

  (* return tuple: (player object * screen * number of sequential good rows) *)
  (player, screen', seq_good_rows')

let start_page () = 
  clear_graph ();
  set_color start_page_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color text_color;
  (* set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";  causes error on windows *)
  let (x1, y1) = text_size "Welcome to Crossy Caml!" in
  moveto ((size_x () - x1) / 2) ((size_y () - y1) / 2);
  draw_string "Welcome to Crossy Caml!";
  let (x2, y2) = text_size "Press the space bar twice to begin" in
  moveto ((size_x () - x2) / 2) (size_y () / 2 - y1 - y2);
  draw_string "Press the space bar twice to begin"

let game_over (score : int) (high_score : int) : unit = 
  clear_graph ();
  set_color start_page_color;
  fill_rect 0 0 (size_x ()) (size_y ());

  set_color text_color;
  (* set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";  causes error on windows *)
  let (x1, y1) = text_size "Game Over" in
  moveto ((size_x () - x1) / 2) ((size_y () - y1) / 2);
  draw_string "Game Over";

  let (x2, y2) = text_size ("Your Score was " ^ string_of_int score) in
  moveto ((size_x () - x2) / 2) (size_y () / 2 - y1 - y2);
  draw_string ("Your Score was " ^ string_of_int score);

  let (x3, y3) = text_size ("Your High Score is " ^ string_of_int high_score) in
  moveto ((size_x () - x3) / 2) (size_y () / 2 - y1 - y2 - y3);
  draw_string ("Your High Score is " ^ string_of_int high_score);

  let (x4, y4) = text_size "Press 'R' twice to Reset" in
  moveto ((size_x () - x4) / 2) (size_y () / 2 - y1 - y2 - y3 - y4);
  draw_string "Press 'R' twice to Reset"

