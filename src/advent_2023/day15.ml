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
