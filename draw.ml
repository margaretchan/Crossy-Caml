open Graphics
open Actor
open Object

let init_window () = 
  open_graph " 700x700";
  set_window_title "Crossy Caml"

let start_page_color = rgb 0 193 0

let text_color = rgb 79 209 201

let background_color = rgb 109 156 243

let player_color = rgb 209 105 154

let update_window player_dir old_pos update_obstacles = 
  (* [grid_x] is the number of pixels in one horizontal unit of the 
       screen grid *)
  let grid_x = (size_x ()) / 25 in
  (* [grid_y] is the number of pixels in one vertical unit of the screen grid *)
  let grid_y = (size_y ()) / 50 in
  (* There is currently no way to set foreground and background colors?? 
     Filling a rectangle is so janky *)
  set_color background_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color player_color;
  (* Player avator movement logic *)
  let new_pos = old_pos + (player_dir * grid_x) in
  (* keeps avatar in screen, loops around *)
  let pos = if new_pos > (size_x ()) then 0 else (
      if new_pos < 0 then size_x () else new_pos) in
  fill_circle new_pos ((size_y ()) / 7) 50;
  pos

let start_page () = 
  clear_graph ();
  set_color start_page_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color text_color;
  let (txt_size_x1, txt_size_y1) = text_size "Welcome to Crossy Caml!" in
  moveto (size_x () / 2 - txt_size_x1) (size_y () / 2 - txt_size_y1);
  draw_string "Welcome to Crossy Caml!";
  set_text_size 20;
  let (txt_size_x2, txt_size_y2) = text_size "Press the space bar twice to begin" in
  moveto (size_x () / 2 - txt_size_x2) (size_y () / 2 - txt_size_y1 - txt_size_y2);
  draw_string "Press the space bar twice to begin"

let game_over () = 
  clear_graph ();
  set_text_size 20;
  let (txt_size_x, txt_size_y) = text_size "Game Over" in
  moveto (size_x () / 2 - txt_size_x) (size_y () / 2 - txt_size_y);
  draw_string "Game Over";
  moveto (size_x () / 2 - txt_size_x) (size_y () / 2 - txt_size_y*2);
  draw_string "Press 'R' to Restart";











