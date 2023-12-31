#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

let parse_line line =
  line
  |> String.split_on_char '@'
  |> List.map (String.split_on_char ',')
  |> List.map (List.map (fun s -> s |> String.trim |> int_of_string))
  |> List.map (fun (x :: y :: z :: []) -> (x, y, z))
  |> (fun (ps :: vs :: []) -> (ps, vs));;

parse_line "19, 13, 30 @ -2,  1, -2";;

let calculate_time value x vx = ((value - x) / vx);;

let min_val = 0 and max_val = 30 in
    let ((x, y, _), (vx, vy, _)) = parse_line "18, 19, 22 @ -1, -1, -2" in
    let max_t =
      list_max
        [calculate_time min_val x vx;
     calculate_time max_val x vx;
     calculate_time min_val y vy;
     calculate_time max_val y vy] in
    ((x , y), ((x + (vx * max_t)), y + (vy * max_t)));;

let intersection ((p0x, p0y), (p1x, p1y)) ((p2x, p2y), (p3x, p3y)) =
  (* A implémenter *)
  0;;
