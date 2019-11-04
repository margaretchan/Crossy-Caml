open Graphics

let init_window () = 
  open_graph " 700x700";
  set_window_title "Crossy Caml"

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

let game_over () = 
  clear_graph ();
  set_text_size 20;
  let (txt_size_x, txt_size_y) = text_size "Game Over" in
  moveto (size_x () / 2 - txt_size_x) (size_y () / 2 - txt_size_y);
  draw_string "Game Over";
  moveto (size_x () / 2 - txt_size_x) (size_y () / 2 - txt_size_y*2);
  draw_string "Press 'R' to Restart";









