#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

module StringsMap = Map.Make(String);;

type part = { x: int; m: int; a: int; s: int; };;
type workflow = string;
type result = Rejected | Accepted;;
type rule_result =
  Workflow of workflow
| Result of result;;
type rule = part -> rule_result option;;

let default (r : rule_result) : rule = (fun _ -> Some r);;
let predicate (pred : part -> bool) (r: rule_result) : rule =
  (fun p -> if (pred p) then
              Some r
            else
              None);;

let parse_result = function
    "A" -> Result Accepted
  | "R" -> Result Rejected
  | w -> Workflow w;;

let parse_rule rule_string =
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

let parse_rules rules =
  let rec loop acc rules =
    match (rules) with
      h::[] -> (acc, h)
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

parse_workflows (read_lines "../../data/day19-example.input")

let parse_rating rating =
  let r = Str.regexp
            "{x=\\([0-9]+\\),m=\\([0-9]+\\),a=\\([0-9]+\\),s=\\([0-9]+\\)}" in
  let _ = Str.search_forward r rating 0
  and g = (fun n -> int_of_string (Str.matched_group n rating)) in
  {x=(g 1); a=(g 2); m=(g 3); s=(g 4)};;

parse_rating "{x=787,m=2655,a=1222,s=2876}";;

let parse_input input =
  let (workflows, ratings_lines) =
    parse_workflows (read_lines input) in
  let ratings = List.map parse_rating ratings_lines in
  (workflows, ratings);;

parse_input "../../data/day19-example.input";;
