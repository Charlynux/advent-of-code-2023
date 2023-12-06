#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

type set_of_cubes = { red: int; blue: int; green : int; };;
let empty_set = {red=0;blue=0;green=0};;
let a_set = { red=1; blue=2; green=3; };;
let another_set = { red=5; blue=6; green=7; };;

let merge (a: set_of_cubes) (b: set_of_cubes) : set_of_cubes =
  match (a, b) with
    ({ red=r1; blue=b1; green=g1; },
     { red=r2; blue=b2; green=g2; })
    -> { red= r1+r2; blue=b1+b2; green=g1+g2; };;

merge a_set another_set;;

let add_to_color (s : set_of_cubes) (color: string) (n: int) =
  match color with
    "blue" -> merge s {blue=n; red=0;green=0;}
  | "red" -> merge s {red=n; blue=0;green=0;}
  | "green" -> merge s{green=n; blue=0;red=0;}
  | _ -> raise Not_found ;;

let color_as_set (color: string) (n: int) =
  match color with
    "blue" -> {blue=n; red=0;green=0;}
  | "red" -> {red=n; blue=0;green=0;}
  | "green" -> {green=n; blue=0;red=0;}
  | _ -> raise Not_found ;;

let read_color (s : string) : set_of_cubes =
  let r = Str.regexp "\\([0-9]+\\) \\(red\\|blue\\|green\\)" in
  let _ = Str.search_forward r s 0
  and color = Str.matched_group 2 s
  and count = int_of_string (Str.matched_group 1 s) in
  color_as_set color count;;

read_color "28 red";;

let read_set (s: string) : set_of_cubes =
  let colors = List.map read_color (String.split_on_char ',' s) in
  List.fold_right merge colors empty_set;;

read_set "28 blue, 12 green, 1 red";;

let is_possible (limit: set_of_cubes) (cubes: set_of_cubes) : bool =
  cubes.red <= limit.red && cubes.blue <= limit.blue && cubes.green <= limit.green;;

let is_possible_part1 = is_possible {red=12; green=13; blue=14};;

is_possible_part1 {red=12; green=15; blue=14};;

type game = {id: int; sets: set_of_cubes list};;

let parse_game (s: string) : game =
let _ = Str.string_match (Str.regexp "Game \\([0-9]+\\): .*") s 0 in
let id =  int_of_string (Str.matched_group 1 s) in
let sets = String.split_on_char ';' s |> List.map read_set in
{id=id; sets= sets};;

parse_game "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";;

[1; 2; 3; 4]
|> List.filter (fun n -> n == 2);;

let solve_part1 (lines : string list) : int =
  lines
|> List.map parse_game
|> List.filter (fun g -> List.for_all is_possible_part1 g.sets)
|> List.map (fun g -> g.id)
  |> List.fold_left (+) 0;;

solve_part1 (read_lines "../../data/day02-example.input");;
solve_part1 (read_lines "../../data/day02.input");;

(* max de chaque sets *)

let max_of_cubes (a: set_of_cubes) (b: set_of_cubes) : set_of_cubes =
  match (a, b) with
    ({ red=r1; blue=b1; green=g1; },
     { red=r2; blue=b2; green=g2; })
    -> { red= (max r1 r2); blue= (max b1 b2); green= (max g1 g2); };;

let solve_part2 (lines : string list) =
  lines
  |> List.map parse_game
  |> List.map (fun g -> List.fold_left max_of_cubes empty_set g.sets)
  |> List.map (fun set -> set.red * set.green * set.blue)
  |> List.fold_left (+) 0;;

solve_part2 (read_lines "../../data/day02-example.input");;
solve_part2 (read_lines "../../data/day02.input");;
