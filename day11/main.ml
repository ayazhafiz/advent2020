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

type seat = Occupied | Empty | Floor

let parse_seat seat =
  match seat with
  | '#' -> Occupied
  | 'L' -> Empty
  | '.' -> Floor
  | _ -> error "Bad state: seat kind unknown"

let explode s = Array.init (String.length s) (String.get s)

let n_occupied seat = match seat with Occupied -> 1 | _ -> 0

let count_occupied plane =
  Array.fold_left
    (fun acc row ->
      let list_total = Array.fold_left ( + ) 0 (Array.map n_occupied row) in
      acc + list_total)
    0 plane

let get_seat plane r c = plane.(r).(c)

let valid_seat plane r c =
  r >= 0 && c >= 0 && r < Array.length plane && c < Array.length plane.(0)

let step_boarding occupied_around map_seat plane =
  Array.mapi
    (fun r row ->
      Array.mapi (fun c seat -> map_seat seat (occupied_around plane r c)) row)
    plane

let print_plane plane =
  Array.iter
    (fun row ->
      Array.iter
        (fun seat ->
          let s =
            match seat with Occupied -> "#" | Empty -> "L" | Floor -> "."
          in
          print_string s)
        row;
      print_newline ();
      flush_all ())
    plane

let rec fix f x =
  let x' = f x in
  if x = x' then x else fix f x'

let main () =
  let lines = read_file "input.txt" in
  let lines = Array.of_list lines in
  let plane = Array.map (fun row -> Array.map parse_seat (explode row)) lines in

  println "Part 1:";
  let occupied_part1 =
    let occupied_around all_seats r c =
      let rec recur total dr dc =
        let nr = r + dr in
        let nc = c + dc in
        match (dr, dc) with
        | 1, 2 -> total
        | dr, 2 -> recur total (dr + 1) (-1)
        | 0, 0 -> recur total 0 1
        | dr, dc when not (valid_seat all_seats nr nc) -> recur total dr (dc + 1)
        | dr, dc -> recur (total + n_occupied all_seats.(nr).(nc)) dr (dc + 1)
      in
      recur 0 (-1) (-1)
    in
    let map_seat seat num_occ_around =
      match (seat, num_occ_around) with
      | Empty, 0 -> Occupied
      | Occupied, n when n >= 4 -> Empty
      | seat, _ -> seat
    in
    count_occupied (fix (step_boarding occupied_around map_seat) plane)
  in
  println (string_of_int occupied_part1);

  println "Part 2:";
  let occupied_part2 =
    let occupied_around all_seats r c =
      let rec recur total dr dc =
        match (dr, dc) with
        | 1, 2 -> total
        | dr, 2 -> recur total (dr + 1) (-1)
        | 0, 0 -> recur total 0 1
        | dr, dc ->
            let rec walk scale =
              let nr = r + (scale * dr) in
              let nc = c + (scale * dc) in
              if not (valid_seat all_seats nr nc) then 0
              else
                match all_seats.(nr).(nc) with
                | Occupied -> 1
                | Empty -> 0
                | Floor -> walk (scale + 1)
            in
            let new_total = total + walk 1 in
            recur new_total dr (dc + 1)
      in
      recur 0 (-1) (-1)
    in
    let map_seat seat num_occ_around =
      match (seat, num_occ_around) with
      | Empty, 0 -> Occupied
      | Occupied, n when n >= 5 -> Empty
      | seat, _ -> seat
    in
    count_occupied (fix (step_boarding occupied_around map_seat) plane)
  in
  println (string_of_int occupied_part2)

let () = main ()
