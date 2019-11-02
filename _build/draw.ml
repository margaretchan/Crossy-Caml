open Graphics

let init_window () = 
  open_graph " 700x700";
  set_window_title "Crossy Caml"

(** colors *)
let light_blue = rgb 109 156 243
let rose = rgb 209 105 154

(** [update_window player_dir old_pos ] is the x position of the player 
    avatar after drawing the updated game graphics 
    [player_dir] is the direction the player moves in:
    | -1 -> left
    | 0 -> no movement
    | 1 -> right 
    [old_pos] is the x position of the player avatar prior to update *)
let update_window player_dir old_pos = 
  (* There is currently no way to set foreground and background colors?? 
     Filling a rectangle is so janky *)
  set_color light_blue;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color rose;
  (* Player avator movement logic *)
  let new_pos = old_pos + (player_dir * (size_x ()) / 25) in
  (* keeps avatar in screen, loops around *)
  let pos = if new_pos > (size_x ()) then 0 else (
      if new_pos < 0 then size_x () else new_pos) in
  fill_circle new_pos ((size_y ()) / 7) 50;
  pos

let game_over = 
  moveto (size_x () / 2) (size_y () / 2);
  set_text_size 20;
  draw_string "Game Over";
  set_color light_blue;
  fill_rect 0 0 (size_x ()) (size_y ());






