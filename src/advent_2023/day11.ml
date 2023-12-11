#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

module Point = struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end
module PointsSet = Set.Make(Point)

module Int = struct
  type t = int
  let compare = compare
end
module IntsSet = Set.Make(Int)

let parse_line y line =
  let rec loop acc x s =
    if (String.length s = 0) then
      acc
    else
      loop
        (match (String.get s 0) with
           '#' -> (x, y)::acc
         | _ -> acc)
        (x + 1)
        (String.sub s 1 ((String.length s) - 1)) in
  loop [] 0 line;;

let parse_map lines =
  let set = lines
  |> List.mapi parse_line
  |> List.fold_left
       (fun acc pairs ->
         List.fold_left
           (fun s coord -> PointsSet.add coord s)
           acc
           pairs
       )
       PointsSet.empty
  and y = List.length lines
  and x = String.length (List.hd lines) in
  ((x, y), set);;

let find_expansions points f size =
  let range = List.init size (fun n -> n) |> IntsSet.of_list
  and vals = PointsSet.to_list points
           |> List.map f
           |> IntsSet.of_list in
  IntsSet.diff range vals |> IntsSet.to_list;;

let expand_all_x expansion_size expansions points =
  points
  |> PointsSet.map
       (fun (px, py) ->
         (let n = List.filter (fun x -> x < px) expansions
                |> List.length in
          (px + (n * (expansion_size - 1)), py)));;

let expand_all_y expansion_size expansions points =
  points
  |> PointsSet.map
       (fun (px, py) ->
         (let n = List.filter (fun y -> y < py) expansions
                  |> List.length in
          (px, py + (n * (expansion_size - 1)))));;


let manhattan_distance (ax, ay) (bx, by) = (abs (bx - ax)) + (abs (by - ay));;

let rec solve_part1 acc points =
  match points with
    [] -> acc
  | p :: rest -> acc @ (solve_part1
                          (List.map (manhattan_distance p) rest)
                          rest);;

let full_solve_part1 expansion_size file =
  let ((x, y), m) = parse_map (read_lines file) in
    let x_expansions = find_expansions m fst x
    and y_expansions = find_expansions m snd x in
    let expanded_map = m
                       |> expand_all_x expansion_size x_expansions
                       |> expand_all_y expansion_size y_expansions
                       |> PointsSet.to_list in
    solve_part1 [] expanded_map
    |> list_sum;;

full_solve_part1 2 "../../data/day11-example.input";;
full_solve_part1 2 "../../data/day11.input";;

full_solve_part1 10 "../../data/day11-example.input";;
full_solve_part1 100 "../../data/day11-example.input";;

full_solve_part1 1000000 "../../data/day11.input";;
