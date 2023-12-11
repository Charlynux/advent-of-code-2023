(*
  F-7
  | |
  L-J

  (x, y)
  - -> (x - 1, y) (x + 1, y)
  | -> (x, y - 1) (x, y + 1)
  F


 *)
let delta ((ax, ay) : int * int) ((bx, by) : int * int) =
  (ax - bx, ay - by);;

let move ((offset_x, offset_y) : int * int) ((x, y) : int * int) =
  (x + offset_x, y + offset_y);;

let dot (coords : int * int) (origin : int * int) =
  raise (Invalid_argument "DON'T MOVE HERE");;

let pipe (coords : int * int) (origin : int * int) =
  match (delta coords origin) with
    (0, 1) ->  move (0, 2) origin
  | (0, -1) -> move (0, -2) origin
  | _ -> raise (Invalid_argument "KO pipe");;

let hyphen (coords : int * int) (origin : int * int) =
  match (delta coords origin) with
    (1, 0) ->  move (2, 0) origin
  | (-1, 0) -> move (-2, 0) origin
  | _ -> raise (Invalid_argument "KO hyphen");;

let bend_south_east (coords : int * int) (origin : int * int) =
  match (delta coords origin) with
    (0, -1) ->  move (1, -1) origin
  | (-1, 0) -> move (-1, 1) origin
  | _ -> raise (Invalid_argument "KO SE");;

let bend_south_west (coords : int * int) (origin : int * int) =
  match (delta coords origin) with
    (0, -1) ->  move (-1, -1) origin
  | (1, 0) -> move (1, 1) origin
  | _ -> raise (Invalid_argument "KO SW");;

let bend_north_west (coords : int * int) (origin : int * int) =
  match (delta coords origin) with
    (0, 1) ->  move (-1, 1) origin
  | (1, 0) -> move (1, -1) origin
  | _ -> raise (Invalid_argument "KO NW");;

let bend_north_east (coords : int * int) (origin : int * int) =
  match (delta coords origin) with
    (0, 1) ->  move (1, 1) origin
  | (-1, 0) -> move (-1, -1) origin
  | _ -> raise (Invalid_argument "KO NE");;

let char_to_function c =
  match c with
    '-' -> hyphen
  | '|' -> pipe
  | '7' -> bend_south_west
  | 'L' -> bend_north_east
  | 'J' -> bend_north_west
  | 'F' -> bend_south_east
  | '.' | _ -> dot;;


[bend_south_east;hyphen;bend_south_west;pipe;bend_north_west;hyphen;bend_north_east]
|> List.rev
|> List.fold_left
     (fun (curr :: prev :: tl) fn ->
       let _ = (print_int (fst curr); print_int (snd curr); print_endline "") in
      (fn curr prev) :: curr :: prev :: tl)
     [(0, 2); (0, 1)];;

module Point = struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end
module PointsMap = Map.Make(Point)

let parse_line y line =
  let rec loop acc x s =
    if (String.length s = 0) then
       acc
    else
      loop
        (((x, y), (String.get s 0))::acc)
        (x + 1)
        (String.sub s 1 ((String.length s) - 1)) in
  loop [] 0 line;;

parse_line 0 ".S-7.";;

let parse_map lines =
  lines
  |> List.mapi parse_line
  |> List.fold_left
       (fun acc pairs ->
         List.fold_left
           (fun m (coord, c) -> PointsMap.add coord c m)
           acc
           pairs
       )
       PointsMap.empty;;

let find_starting_point m = PointsMap.to_list m |> List.find (fun (_, c) -> c = 'S') |> fst;;

let find_connected_neighbors m s =
  [(0, 1); (1, 0); (0, -1); (-1, 0)]
  |> List.map (move s)
  |> List.filter (fun p ->
           match (PointsMap.find_opt p m) with
             None -> false
           | Some c -> (let fn = char_to_function c in
                        try
                        let _ = fn p s in
                        true
                        with Invalid_argument _ -> false));;

let print_point point =
  (print_int (fst point); print_string ",";
   print_int (snd point); print_endline "");;

let follow_pipe_until_s map starting_point neighbor =
  let rec loop path curr prev =
    (*let _ = print_point curr in*)
    if (curr = starting_point) then
      path
    else
      loop
        (curr::path)
        (let fn = char_to_function (PointsMap.find curr map) in
         (fn curr prev))
        curr in
  loop [neighbor; starting_point] neighbor starting_point;;

let solve_part1 file =
  let m = parse_map (read_lines file) in
    let s = find_starting_point m in
    let s_neighbors = find_connected_neighbors m s in
    let path = follow_pipe_until_s m s (List.hd s_neighbors) in
    (List.length path - 1) / 2;;

#trace find_starting_point;;
#trace find_connected_neighbors;;

solve_part1 "../../data/day10-simple.input";;
solve_part1 "../../data/day10-example.input";;
solve_part1 "../../data/day10.input";;

#untrace_all;;

let explore file =
  let m = parse_map (read_lines file) in
  let s = find_starting_point m in
  let s_neighbors = find_connected_neighbors m s in
  let path = follow_pipe_until_s m s (List.hd s_neighbors) in
  let all_coords = PointsMap.to_list m |> List.map fst in
  Printf.printf
    "Taille du plateau %d x %d\nTaille de la boucle %d\nPoints restants %d\n"
    (all_coords |> List.map fst |> list_max)
    (all_coords |> List.map snd |> list_max)
    (List.length path)
    ((List.length all_coords) - (List.length path))
    ;;

explore "../../data/day10.input";;
