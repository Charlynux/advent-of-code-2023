#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

type point = int * int;;
module Point = struct
  type t = point
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end
module PointsMap = Map.Make(Point);;
module PointsSet = Set.Make(Point);;

let parse_line map y line =
  let rec loop acc x s =
    if (String.length s = 0) then
      acc
    else
      loop
        (PointsMap.add (x, y) (String.get s 0) acc)
        (x + 1)
        (String.sub s 1 ((String.length s) - 1)) in
  loop map 0 line;;

let get_max map =
  let coords = map |> PointsMap.to_list |> List.map fst in
  (coords |> List.map fst |> list_max, coords |> List.map snd |> list_max);;

let parse_input lines =
  let rec loop acc y lines =
    match lines with
      [] -> acc
    | line :: rest -> loop
                        (parse_line acc y line)
                        (y + 1)
                        rest in
  let map = loop PointsMap.empty 0 lines in
  map;;

type movement = int * int -> int * int;;

type direction = Right | Left | Bottom | Up;;

module Beam = struct
  type t = point * direction
  let compare ((x0,y0), dir0) ((x1,y1), dir1) =
    match Stdlib.compare x0 x1 with
      0 -> (match Stdlib.compare y0 y1 with
              0 -> Stdlib.compare dir0 dir1
            | c -> c)
    | c -> c
end
module BeamsSet = Set.Make(Beam);;

let print_map energized_beams (map : char PointsMap.t) =
  let (max_x, max_y) = get_max map
  and energized = BeamsSet.to_list energized_beams
                  |> List.map fst
                  |> PointsSet.of_list in
  for y = 0 to max_y do
    for x = 0 to max_x do
      print_char (match ((PointsSet.find_opt (x, y) energized,
                          PointsMap.find_opt (x, y) map)) with
                    (None, None) -> '.'
                  | (Some e, any) -> '#'
                  | (None, Some c) -> c)
    done;
    flush stdout;
    print_endline ""
  done;;

let starting_state = ((0, 0), Right);;

let move dir (x, y) : point =
  match dir with
    Right -> (x + 1, y)
  | Left -> (x - 1, y)
  | Bottom -> (x, y + 1)
  | Up -> (x, y - 1);;

let calculate_next_dirs c dir =
  match c with
    None -> [] (* On ne trouve pas, c'est qu'on est hors-limites. *)
  | Some '.' -> [dir]
  | Some '/' -> (match (dir) with
                   Right -> [Up]
                 | Left -> [Bottom]
                 | Bottom -> [Left]
                 | Up -> [Right])
  | Some '\\' -> (match (dir) with
                    Right -> [Bottom]
                  | Left -> [Up]
                  | Bottom -> [Right]
                  | Up -> [Left])
  | Some '-' -> (match (dir) with
                   Right | Left -> [dir]
                   | Bottom | Up -> [Left; Right])
  | Some '|' -> (match (dir) with
                   Right | Left -> [Bottom; Up]
                   | Bottom | Up -> [dir])
  | Some c -> raise (Invalid_argument (String.make 1 c));;

let step map (pos, dir) : (point * direction) list  =
  let next_pos = move dir pos in
  let next_dirs = calculate_next_dirs (PointsMap.find_opt next_pos map) dir in
  List.map (fun d -> (next_pos, d)) next_dirs;;

let solve_part1 input =
  let m = parse_input (read_lines input) in
    let rec loop energized beams =
      let next_beams = List.concat_map (step m) beams
                       (* On retire les rayons déjà explorés. *)
                       |> BeamsSet.of_list
                       |> (fun elt -> BeamsSet.diff elt energized)
                       |> BeamsSet.to_list in
      (* let _ = print_map energized m; print_endline "" in *)
      let next_energized = List.fold_right
                             BeamsSet.add
                             next_beams
                             energized in
      if (next_energized = energized) then
        energized
      else
        loop next_energized next_beams in
    let energized = loop
                      BeamsSet.empty
                      [((-1, 0), Right)] in
    energized
    |> BeamsSet.to_list
    |> List.map fst
    |> PointsSet.of_list
    |> PointsSet.cardinal;;

solve_part1 "../../data/day16-example.input";;
solve_part1 "../../data/day16.input";;
