#load "str.cma";;

#mod_use "utils.ml";;
open Utils;;

type card_value = int;;

let parse_card (c : char) : card_value =
  match c with
    '2'..'9' -> int_of_string (String.make 1 c)
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> raise (Invalid_argument "Unknown char");;

let parse_hand s =
  String.to_seq s
  |> Seq.fold_left (fun acc c -> acc @ [(parse_card c)]) [];;

(*
  Five of a kind -> 6
  Four of a kind -> 5
  Full house -> 4
  Three of a kind -> 3
  Two pair -> 2
  One pair -> 1
  High card -> 0
 *)

module Int =
  struct
    type t = int
    let compare = compare;
  end;;
module IntMap = Map.Make(Int);;

let frequencies hand =
  hand
  |> List.fold_left
       (fun m v ->
         IntMap.update
           v
           (fun n -> match n with
                       None -> Some 1
                      |Some m -> Some (m + 1))
           m)
       IntMap.empty
  |> IntMap.to_list;;

let evaluate_hand hand =
  match frequencies hand with
    [(_, 5)] -> 6
  | [a; b; c; d; e] -> 0
  | [(_, 4); (_, 1)]
    | [(_, 1); (_, 4)] -> 5
  | [_; _] -> 4
  | f when List.exists (fun (_, n) -> n = 3) f -> 3
  | [_; _; _] -> 2
  | [_; _; _; _] -> 1
  | _ -> raise (Invalid_argument "Impossible hand");;

let rec compare_lists xs ys =
  match (List.hd xs, List.hd ys) with
    (a, b) when a = b -> compare_lists (List.tl xs) (List.tl ys)
  | (a, b) -> compare a b;;

let compare_hands a b =
  let type_a = evaluate_hand a
  and type_b = evaluate_hand b in
  if type_a = type_b then
     compare_lists a b
  else
    compare type_a type_b;;

compare_hands (parse_hand "32T3K") (parse_hand "KK677");;

let parse_line s =
  let hand :: value :: _ = String.split_on_char ' ' s in
  (parse_hand hand, int_of_string value);;

let solve_part1 file = read_lines file
|> List.map parse_line
|> List.sort (fun (hand_a, _) (hand_b, _) -> compare_hands hand_a hand_b)
|> List.mapi (fun i (_, v) -> v * (i + 1))
|> list_sum;;

solve_part1 "../../data/day07-example.input";;
solve_part1 "../../data/day07.input";;

let j_as_joker hand =
  List.map (fun v -> if (v = 11) then 0 else v) hand;;

(*
  Five of a kind -> 6
  Four of a kind -> 5
  Full house -> 4
  Three of a kind -> 3
  Two pair -> 2
  One pair -> 1
  High card -> 0
 *)

let evaluate_hand_with_joker hand =
  match frequencies hand with
    [(_, 5)]
  | [(0, 3); (_, 2)]
  | [(0, 2); (_, 3)]
  | [(0, 1); (_, 4)]
  | [(0, 4); (_, 1)] -> 6

  | [(0, _); b; c; d; e] -> 1
  | [a; b; c; d; e] -> 0

  | [(_, 4); (_, 1)]
    | [(_, 1); (_, 4)] -> 5
  | [_; _] -> 4

  | [(0, 3); _; _] -> 5
  | [(0, 2); _; _] -> 5
  | [(0, 1); (_, n); (_, m)] when (n = 3) || (m = 3) -> 5

  | [(0, 1); (_, 2); (_, 2)] -> 4
  | [(0, 2); _; _; _] -> 3
  | [(0, 1); _; _; _] -> 3

  | (0, _) :: _ -> raise (Invalid_argument ("Unexpected Joker : " ^
                                              (List.fold_left
                                                 (fun acc v ->
                                                   acc ^ (string_of_int v))
                                                 ""
                                                 hand)))
  | f when List.exists (fun (_, n) -> n = 3) f -> 3

  | [_; _; _] -> 2
  | [_; _; _; _] -> 1
  | _ -> raise (Invalid_argument "Impossible hand");;

let compare_hands_with_joker a b =
  let type_a = evaluate_hand_with_joker a
  and type_b = evaluate_hand_with_joker b in
  if type_a = type_b then
    compare_lists a b
  else
    compare type_a type_b;;

let solve_part2 file = read_lines file
                       |> List.map parse_line
                       |> List.map (fun (hand, value) -> (j_as_joker hand, value))
                       |> List.sort
                            (fun (hand_a, _) (hand_b, _) ->
                              compare_hands_with_joker hand_a hand_b)
                       |> List.mapi (fun i (_, v) -> v * (i + 1))
                       |> list_sum;;

solve_part2 "../../data/day07-example.input";;
solve_part2 "../../data/day07.input";;
(* 250950722 -> too low *)
(* 250965300 -> too low *)
(* 251683378 -> too low :'(*)
(* 251548898 -> NOT THE RIGHT *)
(*   252127335 -> SUCCESS *)
