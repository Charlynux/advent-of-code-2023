(*
  - engine part seems to be missing from the engine
  - add up all the part numbers
  - any number adjacent to a symbol, even diagonally,
  is a "part number" and should be included in your sum

Exemple
  467..114..
  ...*......
  ..35..633.
  ......#...
  617*......
  .....+.58.
  ..592.....
  ......755.
  ...$.*....
  .664.598..
--> not part numbers : 114 + 58

Strategy :
- Find numbers and positions (Point to Point)
- Find symbols and positions (Point)
- Calculate numbers neighbors -> Check if symbol in

 *)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop [];;


type point = int * int;;
type number = { value: int; positions : point list; }
type symbol = { value: char; position : point }
type state = {numbers : number list; symbols : symbol list;};;

let neighbors = [(1, 0); (1, -1); (0, -1); (-1, 0); (-1, -1)];;

let is_number (c : char) : bool = '0' <= c && c <= '9';;
let is_symbol (c: char) : bool = c != '.';;

type parsing_state = { number : number option;
                       numbers : number list;
                       symbols : symbol list;};;

let push_digit (n: number option) (d_char: char) (p: point) : number option =
  let d = int_of_string (String.make 1 d_char) in
  match n with
    None -> Some { value = d; positions = [p]; }
  | Some { value = v; positions = ps } ->
     Some { value = ((v * 10) + d); positions = ps @ [p] };;

let parse_line (y: int) (s: string) =
let start_state = { number = None; numbers = []; symbols = []} in
    let rec loop state x s =
      if String.length s = 0 then
        {numbers = (match state.number with
                    None -> state.numbers
                  | Some n -> state.numbers @ [n]);
         symbols = state.symbols}
      else
        let c = (String.get s 0) in
        let updated_state =
          if (is_number c) then
            {numbers = state.numbers;
             symbols = state.symbols;
             number = (push_digit state.number c (x, y)) }
          else
            {numbers = (match state.number with
                          None -> state.numbers
                        | Some n -> state.numbers @ [n]);
             symbols = (if (is_symbol c) then
                          state.symbols @ [{value=c; position=(x, y)}]
                           else
                             state.symbols);
             number = None }
        in
        loop
          updated_state
          (x + 1)
          (String.sub s 1 ((String.length s) - 1));
    in loop start_state 0 s;;

parse_line 0 "467..114..";;
parse_line 0 "617*......";;
parse_line 0 "...664.598";;

let merge (a : state) (b : state) : state =
  match (a, b) with
    ({ numbers = a_numbers; symbols = a_symbols},
     { numbers = b_numbers; symbols = b_symbols})
    -> { numbers = a_numbers @ b_numbers; symbols = a_symbols @ b_symbols};;

let parse_game (lines : string list) =
  lines
  |> List.mapi parse_line
  |> List.fold_left merge { numbers = []; symbols = []};;

parse_game (read_lines "../../data/day03-example.input");;
parse_game (read_lines "../../data/day03.input");;

let game = parse_game (read_lines "../../data/day03.input");;

List.map (fun (n:number) -> n.value) game.numbers;;

let rec first_last myList = match myList with
  | [] -> failwith "too bad"
  | [e] -> (e, e)
  | [e1;e2] -> (e1,e2)
  | e1 :: _ :: r -> first_last (e1::r)

let add (a: point) (b: point) : point =
  match (a, b) with
    ((ax, ay), (bx, by)) -> ((ax + bx), (ay + by));;

let is_in_grid ((x, y) : point) : bool = x >= 0 && y >= 0;;

(*
  Un nombre est positionné sur une ligne de points.
  On calcule l'extérieur des extrémités, avec les positions D et F.
  Puis, on calcule les bords supérieurs et inférieurs pour tous les points, y compris D et F.

  T T T T T T
  D 1 2 3 4 F
  B B B B B B
 *)

module Point = struct
  type t = point
  (*
  let compare (a: point) (b: point) =
    match (a, b) with
      ((ax, ay), (bx, by)) ->
       if (ax = bx) then
         if (ay = by) then 0
         else if (ay < by) then -1
         else 1
       else if (ax < bx) then -1
       else 1
   *)
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
      0 -> Stdlib.compare y0 y1
    | c -> c
end
module Points = Set.Make(Point)


let number_neighbors (positions: point list) =
  let (first, last) = first_last(positions) in
  let (debut, fin) = ((add (-1, 0) first), (add (1, 0) last)) in
  Points.of_list
    (List.filter is_in_grid
        ([debut]
         @ (([debut] @ positions @ [fin])
            |> List.concat_map (fun p -> [(add (0, 1) p); (add (0, -1) p)]))
         @ [fin]));;

let found_part_numbers (s : state) =
  let sym = Points.of_list (List.map (fun s -> s.position) s.symbols) in
  s.numbers
  |> List.filter (fun n ->
         sym
         |> Points.inter (number_neighbors n.positions)
         |> Points.cardinal
         |> (fun n -> n > 0))
  |> List.map (fun (n: number) -> n.value)
  |> List.fold_left (+) 0;;

found_part_numbers (parse_game (read_lines "../../data/day03-example.input"));;
found_part_numbers (parse_game (read_lines "../../data/day03.input"));;
(* 448902 -> too low *)

module DebugMap = Map.Make(Point);;

(* On reconstruit la map pour voir si on a perdu des infos dans le parsing. *)

let add_number_to_map m n =
    let rev = List.rev n.positions in
    let rec loop m n pos =
      if (List.is_empty pos) then
         m
      else
        let d = (n mod 10)
        and next = (n / 10) in
        loop
          (DebugMap.add (List.hd pos) (string_of_int d) m)
          next (List.tl pos) in
      loop m n.value rev;;

let game = parse_game (read_lines "../../data/day03.input");;
let game_map = let m = List.fold_left
          add_number_to_map DebugMap.empty game.numbers in
    List.fold_left
      (fun acc s -> (DebugMap.add s.position (String.make 1 s.value) acc))
      m game.symbols;;

let oc = open_out "../../data/day03.debug" in
    for y = 0 to 139 do
      for x = 0 to 139 do
        Printf.fprintf oc "%s" (match (DebugMap.find_opt (x, y) game_map) with
                                  None -> "."
                                | Some c -> c)
      done;
      Printf.fprintf oc "\n";
    done;
    close_out oc;;

let debug_not_part_numbers (s : state) =
  let sym = Points.of_list (List.map (fun s -> s.position) s.symbols) in
  let not_part = List.filter (fun n ->
                     sym
                     |> Points.inter (number_neighbors n.positions)
                     |> Points.cardinal
                     |> (fun n -> n = 0)) s.numbers in
  List.iter
    (fun n ->
      let positions = (n.positions
                         @ (Points.to_list (number_neighbors n.positions))) in
      let (xs : int list) = List.map fst positions
      and ys = List.map snd positions in
      let x1 = (List.fold_left min (List.hd xs) (List.tl xs))
      and x2 = (List.fold_left max (List.hd xs) (List.tl xs))
      and y1 = (List.fold_left min (List.hd ys) (List.tl ys))
      and y2 = (List.fold_left max (List.hd ys) (List.tl ys)) in
      for y = y1 to y2 do
        for x = x1 to x2 do
          print_string (match (DebugMap.find_opt (x, y) game_map) with
                          None -> "."
                        | Some c -> c)
        done;
        flush stdout;
        print_endline ""
      done;
    )
    not_part;;

List.map (fun s -> (String.make 1 s.value)) game.symbols
|> String.concat ","
|> print_string ;;

debug_not_part_numbers (parse_game (read_lines "../../data/day03.input"));;
