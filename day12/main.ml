(*** UTILS ***)
let read_file filename =
  let lines = ref [] in
  let channel = open_in filename in
  try
    while true do
      lines := input_line channel :: !lines
    done;
    !lines
  with End_of_file ->
    close_in channel;
    List.rev !lines

let println a =
  Format.print_string (a ^ "\n");
  Format.print_flush ()

exception Exit of int

let error a =
  println a;
  raise (Exit 1)

(*** AoC ***)

type command =
  | North of int
  | South of int
  | East of int
  | West of int
  | Rotl of int
  | Rotr of int
  | Fwd of int

let parse_command line =
  let by = int_of_string (String.sub line 1 (String.length line - 1)) in
  match line.[0] with
  | 'N' -> North by
  | 'S' -> South by
  | 'E' -> East by
  | 'W' -> West by
  | 'L' -> Rotl (by / 90)
  | 'R' -> Rotr (by / 90)
  | 'F' -> Fwd by
  | _ -> error "Bad state: not a known command"

let rec rotl by pos =
  match (by, pos) with
  (* (1, 2) -> (-2, 1) -> (-1, -2) -> (2, -1) -> (1, 2) -> ... *)
  | 0, p -> p
  | n, (x, y) when n < 4 -> rotl (n - 1) (-y, x)
  | n, p -> rotl (n mod 4) p

let rotr by pos = rotl (4 - (by mod 4)) pos

let route1 cmds =
  let rec step cmds pos dir =
    match (cmds, pos, dir) with
    | [], pos, _ -> pos
    | North by :: rest, (x, y), d -> step rest (x, y + by) d
    | South by :: rest, (x, y), d -> step rest (x, y - by) d
    | East by :: rest, (x, y), d -> step rest (x + by, y) d
    | West by :: rest, (x, y), d -> step rest (x - by, y) d
    | Rotl by :: rest, p, d -> step rest p (rotl by d)
    | Rotr by :: rest, p, d -> step rest p (rotr by d)
    | Fwd by :: rest, (x, y), ((dx, dy) as d) ->
        step rest (x + (by * dx), y + (by * dy)) d
  in
  step cmds (0, 0) (1, 0)

let route2 cmds =
  let rec step cmds pos dir =
    match (cmds, pos, dir) with
    | [], pos, _ -> pos
    | North by :: rest, p, (wx, wy) -> step rest p (wx, wy + by)
    | South by :: rest, p, (wx, wy) -> step rest p (wx, wy - by)
    | East by :: rest, p, (wx, wy) -> step rest p (wx + by, wy)
    | West by :: rest, p, (wx, wy) -> step rest p (wx - by, wy)
    | Rotl by :: rest, p, wp -> step rest p (rotl by wp)
    | Rotr by :: rest, p, wp -> step rest p (rotr by wp)
    | Fwd by :: rest, (x, y), ((wx, wy) as wp) ->
        step rest (x + (by * wx), y + (by * wy)) wp
  in
  step cmds (0, 0) (10, 1)

let l1 (x, y) = abs x + abs y

let main () =
  let lines = read_file "input.txt" in
  let commands = List.map parse_command lines in

  println "Part 1:";
  println (string_of_int (l1 (route1 commands)));

  println "Part 2:";
  println (string_of_int (l1 (route2 commands)))

let () = main ()
