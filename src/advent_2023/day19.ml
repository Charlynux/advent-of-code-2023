#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

type part = { x: int; m: int; a: int; s: int; };;
type workflow = string;
type result = Rejected | Accepted;;
type rule_result =
  Workflow of workflow
| Result of result;;
type rule_fn = part -> rule_result option;;

module StringsMap = Map.Make(String);;

let predicate (pred : part -> bool) (r: rule_result) : rule_fn =
  (fun p -> if (pred p) then
              Some r
            else
              None);;

let parse_result : string -> rule_result = function
    "A" -> Result Accepted
  | "R" -> Result Rejected
  | w -> Workflow w;;

let parse_rule rule_string : rule_fn =
  try
  let r = Str.regexp "\\([xmas]\\)\\([<>]\\)\\([0-9]+\\):\\([ARa-z]+\\)" in
  let _ = Str.search_forward r rule_string 0 in
    let value = int_of_string (Str.matched_group 3 rule_string)
    and result = parse_result (Str.matched_group 4 rule_string) in
    let partial_rule = match (Str.matched_group 1 rule_string,
                              Str.matched_group 2 rule_string) with
        ("x", ">") -> predicate (fun p -> p.x > value)
      | ("x", "<") -> predicate (fun p -> p.x < value)
      | ("m", ">") -> predicate (fun p -> p.m > value)
      | ("m", "<") -> predicate (fun p -> p.m < value)
      | ("a", ">") -> predicate (fun p -> p.a > value)
      | ("a", "<") -> predicate (fun p -> p.a < value)
      | ("s", "<") -> predicate (fun p -> p.s < value)
      | ("s", ">") -> predicate (fun p -> p.s > value)
      | _ -> raise (Invalid_argument rule_string) in
    partial_rule result
  with
    Not_found -> raise (Invalid_argument rule_string);;

let parse_rules rules : rule_fn list * rule_result =
  let rec loop acc rules =
    match (rules) with
      h::[] -> (acc, (parse_result h))
    | h::tl -> loop (acc@[(parse_rule h)]) tl
    | _ -> raise Not_found in
  loop [] rules;;

let parse_workflow line =
  let r = Str.regexp "\\([a-z]+\\){\\(.*\\)}" in
  let _ = Str.search_forward r line 0 in
  let name = Str.matched_group 1 line
  and rules = Str.matched_group 2 line
              |> String.split_on_char ','
              |> parse_rules
  in
  (name, rules);;

parse_workflow "px{a<2006:qkq,m>2090:A,rfg}";;

let parse_workflows lines =
  let rec loop workflows lines =
    match (lines) with
      [] -> (workflows, [])
    | "" :: tail -> (workflows, tail)
    | w :: tail -> loop
                     (let (name, rules) = parse_workflow w in
                      StringsMap.add name rules workflows)
                     tail in
  loop StringsMap.empty lines;;

parse_workflows (read_lines "../../data/day19-example.input");;

let parse_rating rating =
  let r = Str.regexp
            "{x=\\([0-9]+\\),m=\\([0-9]+\\),a=\\([0-9]+\\),s=\\([0-9]+\\)}" in
  let _ = Str.search_forward r rating 0
  and g = (fun n -> int_of_string (Str.matched_group n rating)) in
  {x=(g 1); m=(g 2); a=(g 3); s=(g 4)};;

parse_rating "{x=787,m=2655,a=1222,s=2876}";;

let parse_input input =
  let (workflows, ratings_lines) =
    parse_workflows (read_lines input) in
  let ratings = List.map parse_rating ratings_lines in
  (workflows, ratings);;

parse_input "../../data/day19-example.input";;

let apply_workflow
      ((rules, default) : rule_fn list * rule_result)
      (part : part) : rule_result =
  let rec loop remaining_rules =
    match remaining_rules with
      [] -> default
    | rule :: tail ->
       (let result : rule_result option = rule part in
        match result with
          None -> loop tail
        | Some r -> r
       ) in
  loop rules;;

let rate workflows part =
  let rec loop = function
      Result r -> r
    | Workflow w ->
       loop
         (apply_workflow
            (StringsMap.find w workflows)
            part) in
  loop (Workflow "in");;

let (ws, parts) = parse_input "../../data/day19-example.input" in
    rate ws {x=2127;m=1623;a=2188;s=1013};;

#untrace rate;;

let solve_part1 input = let (ws, parts) = parse_input input in
    let rating = rate ws in
    parts
    |> List.filter
         (fun p ->
           match rating p with
             Accepted -> true
           | Rejected -> false)
    |> List.map (fun p -> p.x + p.m + p.a + p.s)
    |> list_sum
;;

solve_part1 "../../data/day19-example.input";;
solve_part1 "../../data/day19.input";;
