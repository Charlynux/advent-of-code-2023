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

let parse_input lines =
  let rec loop acc y lines =
    match lines with
      [] -> acc
    | line :: rest -> loop
                        (parse_line acc y line)
                        (y + 1)
                        rest in
  loop PointsMap.empty 0 lines;;

let get_max map =
  let coords = map |> PointsMap.to_list |> List.map fst in
  (coords |> List.map fst |> list_max, coords |> List.map snd |> list_max);;

let down_in_ys max_y map x =
  let rec down lower y map =
    if (y > max_y) then map
    else match (PointsMap.find_opt (x, y) map) with
           Some 'O' when lower = y -> down (y + 1) (y + 1) map
         | Some 'O' -> down (lower + 1) (y + 1) (map
                                                 |> PointsMap.add (x, lower) 'O'
                                                 |> PointsMap.add (x, y) '.')
         | Some '#' -> down (y + 1) (y + 1) map
         | Some '.' -> down lower (y + 1) map
         | Some c -> raise (Invalid_argument (String.make 1 c))
         | None -> raise (Invalid_argument (Printf.sprintf "%d, %d" x y)) in
  down 0 0 map;;

let solve_part1 input =
  let m = parse_input (read_lines input) in
    let (max_x, max_y) = get_max m in
    List.init (max_x + 1) (fun x -> x)
    |> List.fold_left (down_in_ys max_y) m
    |> PointsMap.to_list
    |> List.filter (fun (_, c) -> c = 'O')
    |> List.map (fun ((_, y), _) -> max_y + 1 - y)
    |> list_sum;;

solve_part1 "../../data/day14-example.input";;
solve_part1 "../../data/day14.input";;
