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

type direction = Right | Left | Bottom | Up;;

let parse_line line =
  let r = Str.regexp "\\([LRUD]\\) \\([0-9]+\\) .*" in
  let _ = Str.search_forward r line 0 in
  ((match Str.matched_group 1 line with
      "L" -> Left
    | "R" -> Right
    | "U" -> Up
    | "D" -> Bottom
    | s -> raise (Invalid_argument s)
   ),
   int_of_string (Str.matched_group 2 line));;

parse_line "L 4 (#906400)";;

let step (x, y) (dir, n) =
  match dir with
    Left -> List.init n (fun m -> (x - m - 1, y)) |> List.rev
  | Right -> List.init n (fun m -> (x + m + 1, y)) |> List.rev
  | Up -> List.init n (fun m -> (x, y - m - 1)) |> List.rev
  | Bottom -> List.init n (fun m -> (x, y + m + 1)) |> List.rev;;

let dig input = read_lines input
  |> List.map parse_line
  |> List.fold_left
     (fun l inst -> (step (List.hd l) inst) @ l)
     [(0, 0)]
  |> PointsSet.of_list;;

dig "../../data/day18-example.input" |> PointsSet.to_list;;

let neighbors (x, y) = [(x - 1, y); (x + 1, y);
                        (x, y - 1); (x, y + 1)];;

let fill border =
  let rec loop opens fills =
    let next_fills = PointsSet.union opens fills in
    if (PointsSet.is_empty opens) then
      next_fills
    else
      let next_open = opens
                    |> PointsSet.to_list
                    |> List.concat_map neighbors
                    |> List.filter
                         (fun pos ->
                           not (PointsSet.mem pos border
                             || PointsSet.mem pos next_fills))
                    |> PointsSet.of_list in
      loop next_open next_fills in
  let hole = loop (PointsSet.of_list [(1, 1)]) (PointsSet.of_list [(1, 1)]) in
  PointsSet.union border hole
;;

dig "../../data/day18-example.input" |> fill |> PointsSet.cardinal;;

dig "../../data/day18.input" |> fill |> PointsSet.cardinal;;
