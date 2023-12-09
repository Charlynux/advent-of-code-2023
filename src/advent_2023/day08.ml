#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;


type direction = Left | Right;;
type node = string;;
type instruction = node * node;;

module Node =
  struct
    type t = node
    let compare = compare
  end;;
module NodeMap = Map.Make(Node);;

let get_node (direction : direction) (left, right) =
  match direction with
    Left -> left
  | Right -> right;;

let solve_part1 (map : instruction NodeMap.t) (directions : direction list) =
  let rec loop (ds : direction list) (current_node : node) (n : int) =
    if (List.is_empty ds) then
      loop directions current_node n
    else if (current_node = "ZZZ") then
      n
    else
      loop (List.tl ds)
        (get_node (List.hd ds) (NodeMap.find current_node map))
        (n + 1)
  in
  loop directions "AAA" 0;;

let parse_line s =
  let r = Str.regexp "\\([0-9A-Z]+\\) = (\\([0-9A-Z]+\\), \\([0-9A-Z]+\\))" in
  let _ = Str.search_forward r s 0 in
  ((Str.matched_group 1 s),
   ((Str.matched_group 2 s),
    (Str.matched_group 3 s)));;

let parse_input lines =
  let directions = (List.hd lines)
                   |> String.to_seq
                   |> Seq.map (fun c -> match c with
                                          'L' -> Left
                                         |'R' -> Right
                                         | _ -> raise (Invalid_argument "Ni L ni R"))
                   |> List.of_seq
    and map = lines |> List.tl |> List.tl
      |> List.map parse_line
      |> List.fold_left
           (fun m (node, instruction)
            -> (NodeMap.add node instruction m))
           NodeMap.empty in
  (directions, map);;

let (dirs, map) = parse_input (read_lines "../../data/day08-example.input") in
    solve_part1 map dirs;;

let (dirs, map) = parse_input (read_lines "../../data/day08.input") in
    solve_part1 map dirs;;

module NodeSet = Set.Make(Node);;

let ends_with c node = c = (String.get node 2);;

let get_starting_list map =
  (NodeMap.to_list map
   |> List.map fst
   |> List.filter (ends_with 'A'));;

let get_starting_set map = get_starting_list map |> NodeSet.of_list;;

let get_starting_array map = get_starting_list map |> Array.of_list;;

let print_status count pos node =
  print_int count; print_char ',';
  print_int pos; print_char ',';
  print_string node; print_char ';';
  print_endline "";;

let peek print_fn predicate =
  (fun i node ->
    let result = (predicate node) in
    if result then (print_fn i node);
    result);;

let rec gcd u v =
  if v <> 0 then (gcd v (u mod v))
  else (abs u);;

let lcm m n =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n);;


let solve_part2 (map : instruction NodeMap.t) (directions : direction list) =
  let rec loop (found : (int * int * string) list)
            (ds : direction list) nodes (n : int) =
    if (List.is_empty ds) then
      loop found directions nodes n
    else if
      (let fs = (found
                 |> List.map (fun (_, pos, _) -> pos)
                 |> List.sort_uniq compare |> List.length)
       in fs = Array.length nodes)
    then
      (let positions = List.init (Array.length nodes) (fun x -> x) in
       let hits = positions
                  |> List.map (fun pos -> List.find (fun (_, p, _) -> p = pos) found)
                  |> List.map (fun (n, _, _) -> n) in
      List.fold_left lcm (List.hd hits) (List.tl hits))
    else
       loop
         (found @
            (nodes
             |> Array.to_list
             |> List.mapi (fun pos node -> (n, pos, node))
             |> List.filter (fun (_, _, node) -> (ends_with 'Z' node))))
        (List.tl ds)
        (Array.map
           (fun n -> (get_node (List.hd ds) (NodeMap.find n map)))
           nodes)
        (n + 1)
  in
  loop
    []
    directions
    (get_starting_array map)
    0;;

let (dirs, map) = parse_input (read_lines "../../data/day08-example2.input") in
    solve_part2 map dirs;;

let (dirs, map) = parse_input (read_lines "../../data/day08.input") in
    solve_part2 map dirs;;
