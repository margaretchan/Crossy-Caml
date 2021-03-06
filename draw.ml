open Graphics
open Actor
open Object
open Screen
open Images
open Png

let init_window () = 
  open_graph " 750x750";
  set_window_title "Crossy Caml"

(** [start_page_color] is the background color of the start screen *)
let start_page_color = rgb 105 206 236

(** [title_image_name] is the name of the image file for the title screen *)
let title_image_name = "pngs/title.png"

(** [select_image_name] is the name of the image file for the select screen *)
let select_image_name = "pngs/select.png"

(** [gameover_page_color] is the background color of the game over screen *)
let gameover_page_color = rgb 47 0 31

(** [gameover_image_name] is the name of the image file for the 
    game over screen *)
let gameover_image_name = "pngs/gameover.png"

(** [pause_page_color] is the background color of the pause screen *)
let pause_page_color = rgb 255 238 39

(** [pause_image_name] is the name of the image file for the pause screen *)
let pause_image_name = "pngs/pause.png"

(** [died_page_color] is the background color of the death screen *)
let died_page_color = rgb 255 93 147

(** [died_image_name] is the name of the image file for the death screen *)
let died_image_name = "pngs/died.png"

(** [playerL_image_name] is the name of the player image file pointed to the 
    left *)
let playerL_image_name = "pngs/camelL.png"

(** [playerR_image_name] is the name of the player image file pointed to the 
    right *)
let playerR_image_name = "pngs/camelR.png"

(** [small_badL_image_name] is the name of the image file for a 
    1-wide bad block facing left *)
let small_badL_image_name = "pngs/onebadL.png"

(** [small_badR_image_name] is the name of the image file for a 
    1-wide bad block facing right *)
let small_badR_image_name = "pngs/onebadR.png"

(** [medium_badL_image_name] is the name of the image file for a 
    2-wide bad block facinig left *)
let medium_badL_image_name = "pngs/twobadL.png"

(** [medium_badR_image_name] is the name of the image file for a 
    2-wide bad block facinig right *)
let medium_badR_image_name = "pngs/twobadR.png"

(** [large_badL_image_name] is the name of the image file for a 
    3-wide bad block facing left *)
let large_badL_image_name = "pngs/threebadL.png"

(** [large_badR_image_name] is the name of the image file for a 
    3-wide bad block facing left *)
let large_badR_image_name = "pngs/threebadR.png"

(** [mult_image_name] is the name of the image file for a multiplier item *)
let mult_image_name = "pngs/multiplier.png"

(** [adder_image_name] is the name of the image file for a adder item *)
let adder_image_name = "pngs/adder.png"

(** [freezer_image_name] is the name of the image file for a freezer item *)
let freezer_image_name = "pngs/freezer.png"

(** [life_image_name] is the name of the image file for a life item *)
let life_image_name = "pngs/life.png"

(** [phaser_image_name] is the name of the image file for a phaser item *)
let phaser_image_name = "pngs/phaser.png"

(** [clear_image_name] is the name of the image file for a clear item *)
let clear_image_name = "pngs/clear.png"

(** [speeder_image_name] is the name of the image file for a speeder item *)
let speeder_image_name = "pngs/speeder.png"

(** [subtracter_image_name] is the name of the image file for a 
    subtracter item *)
let subtracter_image_name = "pngs/subtractor.png"

(** [mystery_image_name] is the name of the image file for a mystery item *)
let mystery_image_name = "pngs/mystery.png"

(** [text_color] is the color of the text on the start screen *)
let text_color = rgb 255 255 255

(** [background_color] is the color of the screen background *)
let background_color = rgb 255 207 57

(** [bad_blk_color] is the color of the bad blocks on the screen *)
let bad_blk_color = rgb 23 97 62

(** [good_blk_color] is the color of the passable blocks on the screen *)
let good_blk_color = background_color

(** [item_color] is the color of the item blocks on the screen *)
let item_color = rgb 34 97 186

(** [good_blks_bad_row] is the number of good blocks present in a bad row *)
let good_blks_bad_row = 10

(** [good_blks_good_row] is the number of good blocks present in a good row *)
let good_blks_good_row = 30

(** [apply_transparency img] is an Rgb24 representation of a Png, where the
      transparent pixels are accounted for using the Graphics library unique
      transparent color. 
      note: This function was generously donated by Gonzalo Gonzalez *)
let apply_transparency = function
  | Rgba32 bitmap ->
    let w = bitmap.Rgba32.width
    and h = bitmap.Rgba32.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {color = 
                   {r = r; g = g; b = b}; 
                 alpha = a} = 
              Rgba32.unsafe_get bitmap j i in
            if a = 0 
            then Graphics.transp 
            else rgb r g b))
  | _ -> failwith "impossible - always PNG with RGBA"

(** [get_image png] is the Graphics library image from the [png] *)
let get_image png = 
  png 
  |> apply_transparency
  |> Graphics.make_image

(** [draw_collidable_good_helper goodbad_type obj] draws the item collidable 
    blocks from their png files on the game screen *)
let draw_collidable_item_helper goodbad_type obj = 
  match get_effect goodbad_type with 
  | Adder _ ->       
    let adder_png = Png.load adder_image_name [] in
    let img = get_image adder_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Multiplier _ -> 
    let mult_png = Png.load mult_image_name [] in
    let img = get_image mult_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Phaser _ -> 
    let phaser_png = Png.load phaser_image_name [] in
    let img = get_image phaser_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Slower _ -> 
    let freezer_png = Png.load freezer_image_name [] in
    let img = get_image freezer_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Life _ -> 
    let life_png = Png.load life_image_name [] in
    let img = get_image life_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Clear _ ->       
    let clear_png = Png.load clear_image_name [] in
    let img = get_image clear_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Speeder _ -> 
    let speeder_png = Png.load speeder_image_name [] in
    let img = get_image speeder_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Subtractor _ -> 
    let subtracter_png = Png.load subtracter_image_name [] in
    let img = get_image subtracter_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Mystery _ ->  
    let mystery_png = Png.load mystery_image_name [] in
    let img = get_image mystery_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)
  | Nothing -> ()

(** [draw_collidable_enemy_helper goodbad_type obj] draws the enemy collidable 
    blocks from their png files on the game screen *)
let draw_collidable_enemy_helper goodbad_type obj = 
  let dir = fst obj.velocity in
  let bad_png = 
    match goodbad_type with 
    | SmallB -> 
      if dir = Left then 
        Png.load small_badL_image_name []
      else Png.load small_badR_image_name []
    | MediumB -> 
      if dir = Left then 
        Png.load medium_badL_image_name []
      else Png.load medium_badR_image_name []
    | LargeB -> 
      if dir = Left then 
        Png.load large_badL_image_name []
      else Png.load large_badR_image_name []
    | GoodB _ -> 
      failwith "these should only be bad blocks!" in 
  let img = get_image bad_png in
  Graphics.draw_image img (obj.x_pos) (obj.y_pos)

(** [draw_collidable old_player_dir player_dir collide] draws the collidable 
    object [collide] on the screen as its corresponding png image.
    The color of the rectangle is dependant whether [collide] is a 
    good or bad block type *)
let draw_collidable old_player_dir player_dir collide = 
  match collide with 
  | Block (goodbad_type, obj) -> 
    if Actor.is_good goodbad_type then (
      draw_collidable_item_helper goodbad_type obj
    )
    else  
      draw_collidable_enemy_helper goodbad_type obj

  | Player obj -> 
    let player_png = 
      if player_dir = -1 || (player_dir = 0 && old_player_dir = -1) 
      then Png.load playerL_image_name []
      else Png.load playerR_image_name [] in

    let img = get_image player_png in
    Graphics.draw_image img (obj.x_pos) (obj.y_pos)

(** [moves_player dir step x_bound player] is [player] but 
    moved one [step] in [dir] on a screen with x limit [x_bound]
    Generalize to move_collidable later *)
let moves_player (dir : int) (step : int) (x_bound : int) (player : collidable) 
  : unit  =
  let obj = Object.extract_obj player in 
  (** Loop player position around screen *)
  let pos_after_step = (obj.x_pos) + (dir * step) in
  let new_x_pos = 
    if pos_after_step > x_bound then 
      0 
    else if (pos_after_step + step) < 0 then 
      (x_bound - step) 
    else pos_after_step in
  obj.x_pos <- new_x_pos

(** [get pos c] is the position of collidable [c] *)
let get_pos (c : collidable) =  
  match c with 
  | Player obj -> (obj.x_pos, obj.y_pos)
  | Block (_, obj) -> (obj.x_pos, obj.y_pos)

(** [draw_row collidable_lst] draws the row of collidable blocks *)
let draw_row collidable_lst =
  let rec helper lst = 
    match lst with
    | [] -> ()
    | h :: t -> 
      draw_collidable 1 1 h; 
      helper t in 
  helper (collidable_lst)

(** [draw_desert_background ()] draws the desert background of the game on the 
    screen *)
let draw_desert_background () = 
  Graphics.set_color (rgb 228 174 131);
  Graphics.fill_rect 0 0 200 200;
  Graphics.fill_rect 110 350 200 200;
  Graphics.fill_rect 0 558 200 200;
  Graphics.set_color (rgb 228 174 56);
  Graphics.fill_rect 200 0 200 200;
  Graphics.fill_rect 278 456 50 200;
  Graphics.set_color (rgb 228 174 120);
  Graphics.fill_rect 676 460 100 100;
  Graphics.fill_rect 588 79 200 200;
  Graphics.fill_rect 123 65 200 200;
  Graphics.set_color (rgb 228 174 108);
  Graphics.fill_rect 200 550 200 200;
  Graphics.fill_rect 567 557 200 200;
  Graphics.set_color (rgb 228 174 49);
  Graphics.fill_rect 298 200 200 200;
  Graphics.fill_rect 564 280 200 200;
  Graphics.set_color (rgb 228 174 56);
  Graphics.fill_rect 78 500 200 200;
  Graphics.fill_rect 398 558 200 200;
  Graphics.fill_rect 634 657 200 200;
  Graphics.set_color (rgb 228 174 93);
  Graphics.fill_rect 358 0 200 200;
  Graphics.fill_rect 300 400 200 200;
  Graphics.set_color (rgb 228 174 125);
  Graphics.fill_rect 200 200 200 200;
  Graphics.fill_rect 0 400 200 200;
  Graphics.set_color (rgb 228 174 113);
  Graphics.fill_rect 558 0 200 200;
  Graphics.fill_rect 450 543 200 200;
  Graphics.fill_rect 450 143 150 350;
  Graphics.set_color (rgb 228 174 56);
  Graphics.fill_rect 0 200 200 200;
  Graphics.fill_rect 498 400 200 200

(** [update_game_info_txt player lives] draws the writing on the game screen which 
    displays current lives, score, and effect timers *)
let update_game_info_txt player lives = 
  (* Update Score *)
  let p_obj = Object.extract_obj player in 
  set_color text_color;
  moveto 50 50;
  draw_string ("Score: " ^ (string_of_int p_obj.score));
  (** Update Lives *)
  moveto 50 60;
  draw_string ("Lives: " ^ (string_of_int lives));
  (** Update Effects Drawing *)
  set_color text_color;
  moveto 600 60; 
  draw_string ("Multipler: " ^ 
               (string_of_int (effect_time_left p_obj.effects (Multiplier 0))));
  moveto 600 50; 
  draw_string ("Phaser: " ^ 
               (string_of_int (effect_time_left p_obj.effects (Phaser 0))));
  moveto 600 40; 
  draw_string ("Slower: " ^ 
               (string_of_int (effect_time_left p_obj.effects (Slower 0))));
  moveto 600 30; 
  draw_string ("Speeder: " ^ 
               (string_of_int (effect_time_left p_obj.effects (Speeder 0))))

(** [grid_x ()] is the number of pixels in one horizontal unit of the screen 
    grid *)
let grid_x () = 
  (size_x ()) / 30 

(** [grid_y ()] is the number of pixels in one vertical unit of the screen grid *)
let grid_y () = 
  (size_y ()) / 50 

(** [update_screen side_obstacles down_obstacles screen seq_good_rows] is the 
    updated screen variable (Object.collidable list Queue.t) after the 
    applicable rows have been shifted to the side and down *)
let update_screen side_obstacles down_obstacles screen seq_good_rows = 
  if side_obstacles || down_obstacles then
    let next_row_good = seq_good_rows < 3 in
    let num_good_blks = 
      if down_obstacles then 
        if next_row_good then 
          good_blks_good_row 
        else good_blks_bad_row 
      else 101 in
    Screen.update screen (size_x ()) (size_y ()) num_good_blks (grid_x ()) (grid_y ())
      down_obstacles
  else screen

(** [update_seq_good_rows down_obstacles seq_good_rows] is the number of 
    sequential good rows that should be generated after this iteration *)
let update_seq_good_rows down_obstacles seq_good_rows = 
  if down_obstacles then 
    if (seq_good_rows > 3) then 0 
    else seq_good_rows + 1
  else seq_good_rows

let update_window last_player_dir player_dir (player : collidable) 
    down_obstacles side_obstacles screen seq_good_rows lives = 
  auto_synchronize false;
  Graphics.clear_graph ();
  draw_desert_background ();

  let screen' = 
    update_screen side_obstacles down_obstacles screen seq_good_rows in 
  Queue.iter draw_row screen';

  let seq_good_rows' = update_seq_good_rows down_obstacles seq_good_rows in

  moves_player player_dir (grid_x ()) (size_x ()) player;
  draw_collidable last_player_dir player_dir player;  
  update_game_info_txt player lives;
  auto_synchronize true;

  (player, screen', seq_good_rows')

let start_page () = 
  Graphics.set_color start_page_color;
  Graphics.fill_rect 0 0 750 750;

  auto_synchronize false;

  clear_graph ();

  let title_png = Png.load title_image_name [] in
  let img = get_image title_png in

  Graphics.draw_image img 0 0;

  auto_synchronize true

let pause () = 
  Graphics.set_color pause_page_color;
  Graphics.fill_rect 0 0 750 750;

  auto_synchronize false;
  clear_graph ();

  let pause_png = Png.load pause_image_name [] in
  let img = get_image pause_png in
  Graphics.draw_image img 0 0;

  auto_synchronize true

let select () = 
  Graphics.set_color pause_page_color;
  Graphics.fill_rect 0 0 750 750;

  auto_synchronize false;
  clear_graph ();

  let select_png = Png.load select_image_name [] in
  let img = get_image select_png in
  Graphics.draw_image img 0 0;

  auto_synchronize true

let continue (lives : int) (score : int) = 
  Graphics.set_color died_page_color;
  Graphics.fill_rect 0 0 750 750;

  auto_synchronize false;
  clear_graph ();

  let died_png = Png.load died_image_name [] in
  let img = get_image died_png in
  Graphics.draw_image img 0 0;

  set_color text_color;

  let x_pos_lives = 460 in 
  let y_pos_lives = 304 in 
  moveto x_pos_lives y_pos_lives;
  draw_string (string_of_int lives);

  let x_pos_score = 390 in 
  let y_pos_score = 266 in 
  moveto x_pos_score y_pos_score;
  draw_string (string_of_int score);

  auto_synchronize true

let game_over (score : int) (high_score : int) : unit = 
  Graphics.set_color gameover_page_color;
  Graphics.fill_rect 0 0 750 750;

  auto_synchronize false;
  clear_graph ();

  let gameover_png = Png.load gameover_image_name [] in
  let img = get_image gameover_png in
  Graphics.draw_image img 0 0;

  set_color text_color;

  let x_pos_score = 400 in 
  let y_pos_score = 270 in 
  moveto x_pos_score y_pos_score;
  draw_string (string_of_int score);

  let x_pos_highscore = 445 in 
  let y_pos_highscore = 228 in 
  moveto x_pos_highscore y_pos_highscore;
  draw_string (string_of_int high_score);

  auto_synchronize true
