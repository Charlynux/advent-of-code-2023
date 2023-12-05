type converter = int -> int;;

module Int =
  struct
    type t = int
    let compare = compare;
  end;;
module ConvertingMap = Map.Make(Int);;

let range (start: int) (count : int) = List.init count (fun x -> x + start);;

let create_converter conversion_maps =
  let append map (dest, source, count) =
    List.fold_left2
      (fun acc s d -> (ConvertingMap.add s d acc))
      map
      (range source count)
      (range dest count) in
  let m = (List.fold_left
       append
       ConvertingMap.empty
       conversion_maps) in
  (fun n -> match (ConvertingMap.find_opt n m) with
              None -> n
            | Some v -> v);;

(*
  50 98 2
  52 50 48
 *)

let tmp = create_converter [(50, 98, 2);(52, 50, 48)];;

tmp 0;;
tmp 98;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;

read_lines "../../data/day05-example.input";;

#load "str.cma";;

let parse_seeds s =
  let r = Str.regexp "seeds: \\(.*\\)" in
  let numbers = Str.replace_first r "\\1" s in
  (String.split_on_char ' ' numbers)
  |> List.map int_of_string;;

parse_seeds "seeds: 79 14 55 13";;

let parse_map s =
  let r = Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" in
  let _ = Str.search_forward r s 0 in
  (
    (Str.matched_group 1 s) |> int_of_string,
    (Str.matched_group 2 s) |> int_of_string,
    (Str.matched_group 3 s) |> int_of_string
  );;

let parse_input lines =
  let rec loop converters current ls =
    match ls with
      [] -> converters @ [(create_converter current)]
    | "" :: _ :: tl -> loop (converters @ [(create_converter current)]) [] tl
    | s :: tl -> loop converters (current @ [(parse_map s)]) tl in
  let seeds =  parse_seeds (List.hd lines)
  and converters = lines |> List.tl |> List.tl |> List.tl |> loop [] [] in
  let values =
    (seeds
     |> List.map
          (fun seed ->
            List.fold_left (fun s c -> (c s)) seed converters)) in
  List.fold_left min (List.hd values) (List.tl values);;

parse_input (read_lines "../../data/day05-example.input");;
(*
  IMPLEMENTATION NAIVE KO - GRANDS ENTIERS
  parse_input (read_lines "../../data/day05.input");;
 *)

let is_in_bounds n (_, source, count)= (source <= n) && (n < (source + count));;
let convert n (dest, source, _) = dest + (n - source);;

let handle_seed converters seed =
  match converters |> List.filter (is_in_bounds seed) with
    [] -> seed
  | converter :: [] -> convert seed converter
  | _ -> raise Not_found;;

let parse_input_2 lines =
  let rec loop converters current ls =
    match ls with
      [] -> converters @ [current]
    | "" :: _ :: tl -> loop (converters @ [current]) [] tl
    | s :: tl -> loop converters (current @ [(parse_map s)]) tl in
  let seeds =  parse_seeds (List.hd lines)
  and converters = lines |> List.tl |> List.tl |> List.tl |> loop [] [] in
  (seeds, converters);;

let solve_part1 (seeds, converters) =
  let values =
    List.map
    (fun seed ->
       List.fold_left
       (fun s c -> handle_seed c s)
       seed
       converters)
    seeds in
  List.fold_left min (List.hd values) (List.tl values);;

let total_solve_part1 file = file |> read_lines |> parse_input_2 |> solve_part1;;

total_solve_part1 "../../data/day05-example.input";;
total_solve_part1 "../../data/day05.input";;

let split_seeds seeds =
  let rec loop ranges rest =
    match rest with
      [] -> ranges
    | start :: count :: tl -> loop (ranges @ [(start, count)]) tl
    | _ -> raise Not_found
  in
  loop [] seeds;;

split_seeds (parse_seeds "seeds: 79 14 55 13");;

"../../data/day05.input"
|> read_lines
|> List.hd
|> parse_seeds
|> split_seeds
|> List.map snd
|> List.fold_left (+) 0;;
(* 2 504 127 863 seeds à considérer *)

(*
  dest, source, count + seed, offset
  -> [seed, offset]
  -> [seed, n] [n, m] [m, offset]

  ................seed--------------------------------------seed_end
  source --- end
  ...............................................................source -- end
  source---------------------------------------------------------end
  ........source --- end
  .......................................................source --- end
  ....................... source --- end

 *)

let seed_range_conversion (seed, offset) (dest, source, count) =
  let conversion_end = source + count
  and seed_end = seed + offset in
  match ((seed, offset), (dest, source, count)) with
    _ when conversion_end < seed -> ([], [(seed, offset)])
  | _ when seed_end < source -> ([], [(seed, offset)])
  | _ when (source <= seed)
           && (seed_end <= conversion_end) ->
     ([(dest + (seed - source), offset)], [])
  | _ when (source <= seed)
           && (seed < conversion_end)
           && (conversion_end <= seed_end) ->
     (let cut = conversion_end - seed in
      ([(dest + (seed - source), cut)], [(seed + cut, offset - cut)]))
  | _ when (source <= seed_end)
           && (seed_end <= conversion_end) ->
     (let cut = seed_end - source in
      ([(dest, cut)], [(seed, offset - cut)]))
  | _ -> (let cut = source - seed
          and cut_end = seed_end - conversion_end in
          ([(dest, count)], [(seed, cut); (conversion_end, cut_end + 1)]));;

seed_range_conversion
  (80, 6)
  (25, 82, 2);;

let list_min xs = List.fold_left min (List.hd xs) (List.tl xs);;

let handle_seed_range converters seed_range =
  let rec loop converters (finishes, todos) =
    if (List.length converters = 0) then
      finishes @ todos
    else if (List.length todos = 0) then
      finishes
    else
      let conv = (List.hd converters) in
      let (new_finishes, new_todos) = List.fold_left
                  (fun (fs, tds) t ->
                    (let (nfs, ntds) = seed_range_conversion t conv in
                     (nfs @ fs, tds @ ntds)))
                  (finishes, [])
                  todos in
      loop (List.tl converters) (new_finishes, new_todos)
  in
  loop converters ([], [seed_range]);;

let solve_part2 (seeds, converters) =
  let rec loop converters seed_ranges =
    if (List.length converters = 0) then
      seed_ranges
    else
      loop
        (List.tl converters)
        (List.concat_map
           (fun seed_range ->
             (handle_seed_range (List.hd converters) seed_range))
           seed_ranges)
  in
  loop converters (split_seeds seeds);;

let total_solve_part2 file = file |> read_lines |> parse_input_2 |> solve_part2
                             |> List.map fst
                             |> list_min;;

total_solve_part2 "../../data/day05-example.input";;
total_solve_part2 "../../data/day05.input";;
