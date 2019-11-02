open Graphics

let init_window () = 
  open_graph " 700x700";
  set_window_title "Crossy Caml"

let grid_x = (size_x ()) / 25
let grid_y = (size_y ()) / 50

(** Colors *)
let light_blue = rgb 109 156 243
let rose = rgb 209 105 154 
let update_window player_dir old_pos update_obstacles = 
  (* There is currently no way to set foreground and background colors?? 
     Filling a rectangle is so janky *)
  set_color light_blue;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color rose;
  (* Player avator movement logic *)
  let new_pos = old_pos + (player_dir * grid_x) in
  (* keeps avatar in screen, loops around *)
  let pos = if new_pos > (size_x ()) then 0 else (
      if new_pos < 0 then size_x () else new_pos) in
  fill_circle new_pos ((size_y ()) / 7) 50;
  pos

let game_over = 
  set_text_size 20;
  let (x_size, y_size) = text_size "Game Over" in
  moveto (size_x () / 2 - x_size) (size_y () / 2 - y_size);
  draw_string "Game Over";







