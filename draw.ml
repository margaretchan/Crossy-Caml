open Graphics
open Actor
open Object
open Generator

let init_window () = 
  open_graph " 700x700";
  set_window_title "Crossy Caml"

(** [start_page_color] is the background color of the start screen *)
let start_page_color = rgb 113 182 77

(** [text_color] is the color of the text on the start screen *)
let text_color = rgb 128 0 62

(** [background_color] is the color of the screen background *)
let background_color = rgb 109 156 243

(** [player_color] is the color of the player on the screen *)
let player_color = rgb 209 105 154

(** [bad_blk_color] is the color of the bad blocks on the screen *)
let bad_blk_color = rgb 23 97 62

(** [good_blk_color] is the color of the passable blocks on the screen *)
let good_blk_color = rgb 255 207 57

let draw_collidable blk_width blk_height collide = 
  match collide with 
  | Block (goodbad_type, obj) -> 
    set_color (if goodbad_type = GoodB then good_blk_color else bad_blk_color);
    fill_rect (fst obj.position) (snd obj.position) blk_width blk_height;
  | _ -> failwith "player block type is unimplemented"

let update_window player_dir old_pos update_obstacles = 
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
  set_color player_color;

  (* Draw player *)
  let new_pos = old_pos + (player_dir * grid_x) in
  (* keeps avatar in screen, loops around *)
  let player_pos = if new_pos > (size_x ()) then 0 else (
      if new_pos < 0 then size_x () else new_pos) in
  fill_circle new_pos ((size_y ()) / 7) 50;

  (* Draw block objects *)
  let collidable_lst = Generator.generate (size_x ()) 500 3 grid_x grid_y in 
  List.iter (draw_collidable (2 * grid_x) (2 * grid_y)) collidable_lst;

  (* Return new position of player *)
  player_pos

let start_page () = 
  clear_graph ();
  set_color start_page_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color text_color;
  set_font "-*-fixed-medium-r-semicondensed--40-*-*-*-*-*-iso8859-1";
  let (x1, y1) = text_size "Welcome to Crossy Caml!" in
  moveto ((size_x () - x1) / 2) ((size_y () - y1) / 2);
  draw_string "Welcome to Crossy Caml!";
  let (x2, y2) = text_size "Press the space bar twice to begin" in
  moveto ((size_x () - x2) / 2) (size_y () / 2 - y1 - y2);
  draw_string "Press the space bar twice to begin"

let game_over () = 
  clear_graph ();
  set_text_size 20;
  let (length, height) = text_size "Game Over" in
  moveto (size_x () / 2 - length) (size_y () / 2 - height);
  draw_string "Game Over";
  moveto (size_x () / 2 - length) (size_y () / 2 - height * 2);
  draw_string "Press 'R' to Restart";

