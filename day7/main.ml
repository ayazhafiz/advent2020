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

type bag = Bag of (* bag name *) string * (* contents *) (string * int) list

let parse_bag line =
  let _ = Str.string_match (Str.regexp {|^\([a-z]+ [a-z]+\) bags|}) line 0 in
  let bag_name = Str.matched_group 1 line in
  if Str.string_match (Str.regexp "no other bags") line 0 then Bag (bag_name, [])
  else
    let re_contents = Str.regexp {|[^0-9]*\([0-9]+\) \([a-z]+ [a-z]+\) bag|} in
    let rec parse_contents known search_from =
      if not (Str.string_match re_contents line search_from) then known
      else
        let count = Str.matched_group 1 line in
        let inner_bag = Str.matched_group 2 line in
        parse_contents
          ((inner_bag, int_of_string count) :: known)
          (Str.match_end ())
    in
    Bag (bag_name, parse_contents [] 0)

let print_bag2contents bag2contents =
  Hashtbl.iter
    (fun bag contents ->
      print_string bag;
      print_string " -> ";
      List.iter
        (fun (bag, num) -> print_string (bag ^ ": " ^ string_of_int num))
        contents;
      print_string "\n")
    bag2contents;
  flush_all ()

let print_bag2containing bag2containing =
  Hashtbl.iter
    (fun bag containing ->
      print_string bag;
      print_string " -> ";
      Hashtbl.iter (fun bag _ -> print_string (bag ^ " ")) containing;
      print_string "\n")
    bag2containing;
  flush_all ()

let main () =
  let lines = read_file "input.txt" in
  let bags = List.map parse_bag lines in
  (* Populate bag -> which bags it contains *)
  let bag2contents = Hashtbl.create (List.length bags) in
  List.iter
    (fun (Bag (bag, contents)) -> Hashtbl.add bag2contents bag contents)
    bags;
  (* Populate bag -> which bags contain it *)
  let bag2containing = Hashtbl.create (List.length bags) in
  List.iter
    (fun (Bag (bag, contents)) ->
      Hashtbl.add bag2containing bag (Hashtbl.create 2))
    bags;
  List.iter
    (fun (Bag (bag, contents)) ->
      List.iter
        (fun (inner_bag, _) ->
          let inner_bag_contained_by = Hashtbl.find bag2containing inner_bag in
          if not (Hashtbl.mem inner_bag_contained_by bag) then
            Hashtbl.add inner_bag_contained_by bag true
          else ())
        contents)
    bags;

  (* print_bag2contents bag2contents; *)
  (* print_bag2containing bag2containing; *)
  println "Part 1:";
  let num_containing_shiny_gold =
    let containing = Hashtbl.create (List.length bags) in
    let rec recur to_visit =
      match to_visit with
      | [] -> ()
      | cur :: rest ->
          let to_visit =
            Hashtbl.fold
              (fun outer_bag _ unvisited ->
                if Hashtbl.mem containing outer_bag then unvisited
                else (
                  Hashtbl.add containing outer_bag true;
                  unvisited @ [ outer_bag ] ))
              (Hashtbl.find bag2containing cur)
              rest
          in
          recur to_visit
    in
    recur [ "shiny gold" ];
    Hashtbl.length containing
  in
  println (string_of_int num_containing_shiny_gold);

  println "Part 2:";
  let num_inside_shiny_gold =
    let rec recur bag =
      1
      + List.fold_left
          (fun total (inner, num) -> total + (num * recur inner))
          0
          (Hashtbl.find bag2contents bag)
    in
    recur "shiny gold" - 1
  in
  println (string_of_int num_inside_shiny_gold)

let () = main ()
