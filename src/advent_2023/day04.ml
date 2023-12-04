let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

module Int =
  struct
    type t = int
    let compare = compare;
    end;;
module Ints = Set.Make(Int);;

#load "str.cma";;

let parse_nums s =
  s
  |> String.split_on_char ' '
  |> List.map String.trim
  |> List.filter (fun n -> String.length n > 0)
  |> List.map int_of_string
  |> Ints.of_list;;

let count_points numbers =
  let c = (Ints.cardinal numbers) in
  if (c = 0) then 0 else int_of_float (2. ** (float_of_int (c - 1)));;

let solve_line line =
  let r = Str.regexp "Card +\\([0-9]+\\): \\(.*\\) | \\(.*\\)" in
  let _ = Str.search_forward r line 0 in
  let winnings = parse_nums (Str.matched_group 2 line)
  and mines = parse_nums (Str.matched_group 3 line) in
  Ints.inter winnings mines
  |> count_points;;

solve_line "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";;

read_lines "../../data/day04-example.input"
|> List.map solve_line
|> List.fold_left (+) 0;;

read_lines "../../data/day04.input"
|> List.map solve_line
|> List.fold_left (+) 0;;
