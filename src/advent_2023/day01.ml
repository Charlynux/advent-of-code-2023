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

let chars_to_number cs =
  int_of_string (implode_char_list [(last cs); (List.hd cs)]);;

let solve_part1 lines =
  lines
  |> List.map extract_digits
  |> List.map chars_to_number
  |> List.fold_left (+) 0;;

solve_part1 ["1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet"];;

solve_part1 (read_lines "../../data/day01.input");;

let extract_numbers_words (s: string) =
  let r = Str.regexp
            "^\\([0-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\)" in
  let rec loop acc s =
    if String.length s == 0 then
      acc
    else
      try
        let _ = Str.search_forward r s 0
        and i = Str.matched_group 1 s in
        loop
          (i::acc)
          (String.sub s 1 ((String.length s) - 1))
      with
        Not_found -> loop
                       acc
                       (String.sub s 1 ((String.length s) - 1)) in
  let res = loop [] s in
  if List.length res == 0 then
    raise (Invalid_argument s)
  else
    res;;

let char_of_word (s: string) =
      match s with
        "one" | "1" -> '1'
      | "two" | "2" -> '2'
      | "three" | "3" -> '3'
      | "four" | "4" -> '4'
      | "five" | "5" -> '5'
      | "six" | "6" -> '6'
      | "seven" | "7" -> '7'
      | "eight" | "8" -> '8'
      | "nine" | "9" -> '9'
      | _ -> raise Not_found;;

extract_numbers_words "eightwothree"

let solve_part2 lines =
  lines
  |> List.map (fun s -> List.map char_of_word
                          (extract_numbers_words s))
  |> List.map chars_to_number
  |> List.fold_left (+) 0;;

solve_part2 (read_lines "../../data/day01.input");;
