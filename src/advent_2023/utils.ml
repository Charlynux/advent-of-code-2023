let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

let list_sum = List.fold_left (+) 0;;
let list_min xs = List.fold_left min (List.hd xs) (List.tl xs);;
let list_max xs = List.fold_left max (List.hd xs) (List.tl xs);;
