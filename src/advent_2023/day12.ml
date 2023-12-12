(*
  3 2 1

  ? ### ? ???????

  # -> #### 3 -> KO
  . -> ### . 3 -> OK
  .###.

  ??? ????
  # -> ## -> ##. ???? -> #...
 *)

let explode s = List.init (String.length s) (String.get s);;

let calculate_arrangements pattern searched_groups =
  let rec loop current_group_size groups chars =
    match chars with
      [] -> (match groups with
               [] -> 1
             | n::[] when n = current_group_size -> 1
             | _ -> 0)
    | '#'::tl ->
       if (List.is_empty groups) then
         0
       else if (current_group_size < (List.hd groups)) then
         loop (current_group_size + 1) groups tl
       else
         0
    | '.'::tl ->
       if (current_group_size = 0) then
         loop current_group_size groups tl
       else if (current_group_size = (List.hd groups)) then
         loop 0 (List.tl groups) tl
       else
         0
    | '?'::tl -> ((loop current_group_size groups ('#'::tl))
                  +
                    (loop current_group_size groups ('.'::tl)))
    | _ -> raise Not_found in loop 0 searched_groups (explode pattern);;

calculate_arrangements "???.###" [1;1;3];; (* 1 arrangement *)
calculate_arrangements ".??..??...?##." [1;1;3];; (* 4 arrangements *)
calculate_arrangements "?#?#?#?#?#?#?#?" [1;3;1;6];; (* 1 arrangement *)
calculate_arrangements "????.#...#..." [4;1;1];; (* 1 arrangement *)
calculate_arrangements "????.######..#####." [1;6;5];; (* 4 arrangements *)
calculate_arrangements "?###????????" [3;2;1];; (* 10 arrangements *)

let parse_line line =
  let pattern::groups::[] = String.split_on_char ' ' line in
  (pattern,
   groups |> String.split_on_char ',' |> List.map int_of_string);;

parse_line "?###???????? 3,2,1";;

let solve_part1 input =
  read_lines input
  |> List.map parse_line
  |> List.map (fun (pattern, groups) -> calculate_arrangements pattern groups)
  |> list_sum;;

solve_part1 "../../data/day12-example.input";;
solve_part1 "../../data/day12.input";;
