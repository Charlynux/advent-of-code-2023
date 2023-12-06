let calculate_distance duration hold =
  let remaining_time = duration - hold in
  remaining_time * hold;;

calculate_distance 7 4;;

let solve_part1 (duration, record) =
    let times = List.init (duration + 1) (fun x -> x) in
    times
    |> List.map (calculate_distance duration)
    |> List.filter (fun distance -> distance > record)
    |> List.length;;

(*
  Time:      7  15   30
  Distance:  9  40  200
 *)
let day06_example = [(7, 9); (15, 40); (30, 200)];;
(*
  Time:        44     70     70     80
  Distance:   283   1134   1134   1491
 *)
let day06 = [(44, 283); (70, 1134); (70, 1134); (80, 1491)];;

day06
|> List.map solve_part1
|> List.fold_left ( * ) 1;;

solve_part1 (71530, 940200);; (* 71503 *)

let solve_part2 (duration, record) =
  let rec find_first f n =
    if (record >= calculate_distance duration n) then
      find_first f (f n)
    else
      n in
  let min = find_first (fun x -> x + 1) 0
  and max = find_first (fun x -> x - 1) duration in
  max - min + 1;;

solve_part2 (71530, 940200);; (* 71503 *)
solve_part2 (44707080, 283113411341491);; (* TODO *)
