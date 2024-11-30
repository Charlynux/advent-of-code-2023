#load "str.cma";;
#mod_use "utils.ml";;
open Utils;;

let explode_string s = List.init (String.length s) (String.get s);;
let implode_char_list l = String.of_seq (List.to_seq l);;

let rec last xs =
  match xs with
    [] -> raise Not_found
  | h::[] -> h
  | _::t -> last t;;

let extract_digits (s : string) =
  let rec loop acc cs =
    match cs with
    [] -> acc
    | h::t when int_of_char h >= 48 && int_of_char h <= 57
      -> loop (h::acc) t
    | h::t
      -> loop acc t in
  loop [] (explode_string s);;

let solve_part1 lines =
  lines
  |> List.map extract_digits
  |> List.map
       (fun cs -> int_of_string (implode_char_list [(last cs); (List.hd cs)]))
  |> List.fold_left (+) 0;;

solve_part1 ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"];;

solve_part1 (read_lines "../../data/day01.input");;
