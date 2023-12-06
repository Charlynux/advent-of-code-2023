#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

module Int =
  struct
    type t = int
    let compare = compare;
    end;;
module Ints = Set.Make(Int);;

module CardId =
  struct
    type t = int
    let compare = compare;
  end;;
module CardIds = Map.Make(CardId);;

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
|> list_num;;

read_lines "../../data/day04.input"
|> List.map solve_line
|> list_num;;

let add_opt (n : int) (m_opt : int option) =
  match m_opt with
    None -> n
  | Some m -> m + n;;

List.init 4 (fun x -> x + 1);;

CardIds.update 1 (fun x -> Some 0) CardIds.empty;;

let solve_line_part2 acc line =
  let r = Str.regexp "Card +\\([0-9]+\\): \\(.*\\) | \\(.*\\)" in
  let _ = Str.search_forward r line 0 in
  let id = int_of_string (Str.matched_group 1 line)
  and winnings = parse_nums (Str.matched_group 2 line)
  and mines = parse_nums (Str.matched_group 3 line) in
  let copies = CardIds.find_opt id acc |> (add_opt 1)
  and n = (Ints.inter winnings mines |> Ints.cardinal) in
  List.init n (fun x -> x + id + 1)
  |> List.fold_left
       (fun m i -> (CardIds.update i (fun x -> Some (add_opt copies x)) m))
       acc;;

solve_line_part2 CardIds.empty "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
|> (fun x -> solve_line_part2 x "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")
|> CardIds.to_list;;

let solve_part2 lines =
  lines
  |> List.fold_left solve_line_part2 CardIds.empty
  |> CardIds.to_list
  |> List.map snd
  |> List.fold_left (+) (List.length lines);;

solve_part2 (read_lines "../../data/day04-example.input");;
solve_part2 (read_lines "../../data/day04.input");;
