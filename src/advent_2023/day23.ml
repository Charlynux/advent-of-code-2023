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

let voisins map (x,y) =
  match PointsMap.find (x,y) map with
    '>' -> [(x + 1, y)]
  | '<' -> [(x - 1, y)]
  | '^' -> [(x, y - 1)]
  | 'v' -> [(x, y + 1)]
  | _ -> [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)];;

type result = Echec | Chemins of ((int * int) list) list

let merge_results r1 r2 =
   match (r1, r2) with
      (_, Echec) -> r1
     |(Echec, _) -> r2
     |(Chemins a, Chemins b) -> Chemins (a @ b);;

let find_paths map starting ending =
  let (max_x, max_y) = get_max map in
  let in_bounds (x, y) = x >= 0 && x <= max_x && y >= 0 && y <= max_y in
  let rec loop (current_path : (int * int) list) closeds (x, y) =
    if (x, y) = ending then
      (Chemins [current_path])
    else
      let opens = voisins map (x, y)
                  |> List.filter in_bounds
                  |> List.filter (fun pos -> not(PointsSet.mem pos closeds)) in
      match opens with
        [] -> Echec
      | _ -> opens
             |> List.map (loop
                            (current_path @ [(x, y)])
                            (PointsSet.add (x, y) closeds))
             |> List.fold_left
                  merge_results
                  Echec in
  loop
    []
    (map
     |> PointsMap.to_list
     |> List.filter (fun (pos, value) -> value = '#')
     |> List.map (fun (pos, _) -> pos)
     |> PointsSet.of_list)
    starting;;

let solve_part1 input =
  let map = parse_input (read_lines input) in
  let (x, y) = get_max map in
  (find_paths
     map
     (0, 0)
     (x - 1, y))
  |> (fun x -> match x with
                 Chemins ps -> ps
               | _ -> raise Not_found)
  |> List.map (fun p -> (List.length p) - 1)
  |> list_max;;

solve_part1 "../../data/day23-example.input";;
solve_part1 "../../data/day23.input";;
(* Très lent mais résult OK *)
