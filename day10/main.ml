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

let main () =
  let lines = read_file "input.txt" in
  let adapters = List.map int_of_string lines in
  let adapters = List.sort compare adapters in
  let max_adapter = List.nth adapters (List.length adapters - 1) in
  let adapters = adapters @ [ max_adapter + 3 ] in
  println "Part 1:";
  let diff1, diff3, _ =
    List.fold_left
      (fun (diff1, diff3, last) cur ->
        match cur - last with
        | 1 -> (diff1 + 1, diff3, cur)
        | 3 -> (diff1, diff3 + 1, cur)
        | _ -> error "Bad state: not 1 or 3")
      (0, 0, 0) adapters
  in
  println (string_of_int (diff1 * diff3));

  println "Part 2:";
  let tbl = Hashtbl.create max_adapter in
  Hashtbl.add tbl 0 1;
  let connect_ways =
    let rec recur cur_adapter =
      if not (List.mem cur_adapter adapters) then recur (cur_adapter + 1)
      else
        let ways =
          List.fold_left
            (fun acc m ->
              acc
              +
              match Hashtbl.find_opt tbl (cur_adapter - m) with
              | Some v -> v
              | None -> 0)
            0 [ 1; 2; 3 ]
        in
        Hashtbl.add tbl cur_adapter ways;
        if cur_adapter == max_adapter then ways else recur (cur_adapter + 1)
    in
    recur 1
  in
  println (string_of_int connect_ways)

let () = main ()
