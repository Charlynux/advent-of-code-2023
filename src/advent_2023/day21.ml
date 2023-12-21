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
module PointsSet = Set.Make(Point);;

let parse_line set y line =
  let rec loop x s start acc =
    if (String.length s = 0) then
      (start, acc)
    else
      let looping = loop (x + 1) (String.sub s 1 ((String.length s) - 1)) in
      match (String.get s 0) with
        '.' -> looping start acc
      | '#' -> looping start (PointsSet.add (x, y) acc)
      | 'S' -> looping (Some (x, y)) acc
      | _ -> raise (Invalid_argument s) in
  loop 0 line None set;;

let parse_input lines =
  let rec loop start acc y lines =
    match lines with
      [] -> (start, acc, y)
    | line :: rest ->
       (let (starting_point, new_acc) = (parse_line acc y line) in
        loop
          (match starting_point with
           None -> start
         | Some _ -> starting_point)
         new_acc (y + 1)rest) in
  let (start, set, max_y) = loop None PointsSet.empty 0 lines in
  (start, (String.length (List.nth lines 0), max_y), set);;

parse_input (read_lines "../../data/day21-example.input");;
parse_input (read_lines "../../data/day21.input");;
