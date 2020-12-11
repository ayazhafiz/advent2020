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

type inst = Acc of int | Jmp of int | Nop of int

let print_inst i =
  match i with
  | Acc i -> println ("acc " ^ string_of_int i)
  | Jmp i -> println ("jmp " ^ string_of_int i)
  | Nop i -> println ("nop " ^ string_of_int i)

let parse_inst s =
  let r = Str.regexp {|^\(acc\|jmp\|nop\) \(+\|-\)\([0-9]+\)$|} in
  let _ = Str.string_match r s 0 in
  let num = int_of_string (Str.matched_group 3 s) in
  let num =
    match Str.matched_group 2 s with
    | "+" -> num
    | "-" -> -1 * num
    | _ -> error "Bad state: sign not + or -"
  in
  match Str.matched_group 1 s with
  | "acc" -> Acc num
  | "jmp" -> Jmp num
  | "nop" -> Nop num
  | _ -> error "Bad state: instruction not known"

type eval_result = FixedPoint of (* line *) int * int | Terminated of int

let counter_of_result r =
  match r with
  | FixedPoint (_, counter) -> counter
  | Terminated counter -> counter

let eval instructions =
  let num_inst = Array.length instructions in
  let seen = Hashtbl.create num_inst in
  let rec eval curline counter =
    if curline >= num_inst then Terminated counter
    else if Hashtbl.mem seen curline then FixedPoint (curline, counter)
    else (
      Hashtbl.add seen curline true;
      match instructions.(curline) with
      | Acc by -> eval (curline + 1) (counter + by)
      | Jmp by -> eval (curline + by) counter
      | Nop _ -> eval (curline + 1) counter )
  in
  eval 0 0

let remove_fixed_point instructions =
  let rec try_candidate prev old nu after =
    let cand = Array.of_list (prev @ [ nu ] @ after) in
    match eval cand with
    | FixedPoint _ -> recur (prev @ [ old ]) after
    | Terminated _ -> cand
  and recur prev instrs =
    match instrs with
    | [] -> raise Not_found
    | cur :: rest -> (
        match cur with
        | Acc _ -> recur (prev @ [ cur ]) rest
        | Jmp by -> try_candidate prev cur (Nop by) rest
        | Nop by -> try_candidate prev cur (Jmp by) rest )
  in
  recur [] instructions

let main () =
  let lines = read_file "input.txt" in
  let instructions_l = List.map parse_inst lines in
  let instructions = Array.of_list instructions_l in
  println "Part 1:";
  println (string_of_int (counter_of_result (eval instructions)));

  println "Part 2:";
  let nofp = remove_fixed_point instructions_l in
  println (string_of_int (counter_of_result (eval nofp)))

let () = main ()
