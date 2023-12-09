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

let get_starting_set map =
  (NodeMap.to_list map
   |> List.map fst
   |> List.filter (ends_with 'A')
   |> NodeSet.of_list);;

let solve_part2 (map : instruction NodeMap.t) (directions : direction list) =
  let rec loop (ds : direction list) nodes (n : int) =
    if (List.is_empty ds) then
      loop directions nodes n
    else if (NodeSet.for_all
               (ends_with 'Z')
               nodes) then
      n
    else if (n > 200000) then
      n
     else
      loop
        (List.tl ds)
        (NodeSet.map
           (fun n -> (get_node (List.hd ds) (NodeMap.find n map)))
           nodes)
        (n + 1)
  in
  loop
    directions
    (get_starting_set map)
    0;;

let (dirs, map) = parse_input (read_lines "../../data/day08-example2.input") in
    solve_part2 map dirs;;

(*
  let (dirs, map) = parse_input (read_lines "../../data/day08.input") in
  solve_part2 map dirs;;


 *)
