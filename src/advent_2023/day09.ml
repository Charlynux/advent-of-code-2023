#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

(*
  0 3 6 9 12 15
    3 3 3  3  3
--> All zero -> extrapolate -> sum
 *)

let parse_line s =
  String.split_on_char ' ' s
  |> List.map int_of_string;;

let parse_input file = file |> read_lines |> List.map parse_line;;

parse_input "../../data/day09-example.input";;
parse_input "../../data/day09.input" |> List.hd;;

let process_line numbers =
  let rec loop result source =
    if (List.length source < 2) then
      result
    else
      loop
        (result @ [(List.hd (List.tl source)) - (List.hd source)])
        (List.tl source) in
  loop [] numbers;;

let process numbers =
  let rec loop history line =
    if (List.for_all (fun n -> n = 0) line) then
      history
    else
      loop
        (line :: history)
        (process_line line) in
  loop [] numbers;;

process (parse_input "../../data/day09.input" |> List.hd);;

let rec extrapolate_one target numbers =
  match numbers with
    [] -> raise Not_found;
  | [x] -> x + target
  | _ :: tl -> extrapolate_one target tl;;

extrapolate_one 3 [5; 15];;

let solve_part1 file =
  file
|> parse_input
|> List.map process
|> List.map (List.fold_left extrapolate_one 0)
|> list_sum;;

solve_part1 "../../data/day09-example.input";;
solve_part1 "../../data/day09.input";;

let extrapolate_back target (x :: _) = -(target - x);;

let solve_part2 file =
  file
  |> parse_input
  |> List.map process
  |> List.map (List.fold_left extrapolate_back 0)
  |> list_sum;;

  #trace extrapolate_back;;
  #untrace extrapolate_back;;

solve_part2 "../../data/day09-example.input";;
solve_part2 "../../data/day09.input";;
