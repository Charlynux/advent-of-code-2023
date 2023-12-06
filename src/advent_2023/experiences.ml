print_string "Hello world!\n";;

let file = "../../data/day01.input";;

let ic = open_in file in
    try
        while true; do
          let line = input_line ic in
          print_endline line;
        done;
        flush stdout;
    with
    | End_of_file -> close_in ic
    | e ->
      close_in_noerr ic;
      raise e;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

let string_example = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";;

let xs = String.split_on_char ';' string_example in
    List.map String.trim xs;;

String.split_on_char ';' string_example
|> List.map String.trim;;

#load "str.cma";;

let r = Str.regexp "hello \\([A-Za-z]+\\)" in
    Str.replace_first r "\\1" "hello world";;

let r = Str.regexp "red \\([0-9]+\\)" in
    let _ = Str.search_forward r "blue 24 red 12" 0 in
    Str.matched_string "blue 24 red 12";;

int_of_string "12";;
