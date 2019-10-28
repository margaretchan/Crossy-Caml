open Graphics

(** [init_window ()] opens the game window *)
let init_window () = 
  open_graph " 700x700";
  set_window_title "Crossy Caml"

(** [update_window ()] draws the game graphics *)
let update_window () = 
  let light_blue = rgb 109 156 243; in
  let rose = rgb 209 105 154; in
  set_color light_blue;
  (* There is currently no way to set foreground and background colors?? *)
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color rose;
  fill_circle ((size_x ()) / 2) ((size_y ()) / 7) 50;
  set_color foreground
