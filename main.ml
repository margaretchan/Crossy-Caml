open Graphics
open Draw

let rec display () = 
  let () = Draw.draw () in
  match read_line () with
  | exception End_of_file -> display ()
  | x -> if x = "done" then () else display ()

let _ = print_endline "hi"

let () = display ()