#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

let explode_string s = List.init (String.length s) (String.get s);;

let step value code =
  ((value + code) * 17) mod 256;;

let hash_algorithm  s =
  s
|> explode_string
|> List.map Char.code
|> List.fold_left step 0;;

let solve_part1 input = String.split_on_char ',' input
|> List.map hash_algorithm
|> list_sum;;

solve_part1 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";;
solve_part1 (List.hd (read_lines "../../data/day15.input"));;

type op =
  Equal of string * int
  | Dash of string;;

let parse_operation s =
  let r = Str.regexp "\\([a-z]+\\)\\(-\\|=[0-9]+\\)" in
  let _ = Str.search_forward r s 0 in
  let label = Str.matched_group 1 s in
  match Str.matched_group 2 s with
    "-" -> Dash label
  | operation -> Equal (label,
                      (int_of_string (String.sub operation 1
                                        (String.length operation - 1))));;

module Int =
  struct
    type t = int
    let compare = compare;
  end;;
module IntMap = Map.Make(Int);;

let apply_operation map operation =
  match operation with
    Dash label -> IntMap.update
                    (hash_algorithm label)
                    (fun found ->
                      match found with
                      None -> None
                      | Some list -> Some (List.filter (fun (l, _) -> l <> label) list))
                    map
  | Equal (label, value) ->
     IntMap.update
       (hash_algorithm label)
       (fun found ->
         match found with
         None -> Some [(label, value)]
       | Some list ->
          Some (if (List.exists (fun (l, _) -> l = label) list) then
             List.map (fun (l, v) ->
                 if (l = label) then (l, value) else (l,v)) list
           else
             list @ [(label, value)]))
       map;;

let focussing_power box slot (_,length) = (box + 1) * (slot + 1) * length;;

let solve_part2 input =
  String.split_on_char ',' input
|> List.map parse_operation
|> List.fold_left apply_operation IntMap.empty
|> IntMap.to_list
|> List.concat_map (fun (box, lenses) -> List.mapi (focussing_power box) lenses)
|> list_sum;;

solve_part2 "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";;
solve_part2 (List.hd (read_lines "../../data/day15.input"));;
