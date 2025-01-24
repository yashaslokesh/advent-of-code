let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  (* List.rev to get the lines in order *)
  with End_of_file -> List.rev l
;;

let part_1_dirs = [| 
  (1,0); (-1, 0); (* Horizontal *)
  0, 1; 0, -1; (* Vertical *)
  1, 1; 1, -1; (* left-down and left-up *)
  -1, 1; -1, -1; (* right-down and right-up *)
  |]

(* These directions are used to go up/down/left/right to the start of a new M *)
(* when a new M is reached, then MAS will be scanned for again *)
let part_2_2nd_word_dirs = [|
  2, 0; -2, 0; (* Horizontal *)
  0, 2; 0, -2; (* Vertical *)
  |]

let part_1 lines = 
  (* Limits to a length of four anyway *)
  let rec construct_word lines line_num char_idx horiz_inc vert_inc word =
    (* We haven't reached 'XMAS' or reverse yet, keep going *)
    if String.length word <> 4 then
      match List.nth_opt lines line_num with
      | Some line -> (
        match line.[char_idx] with
        | c -> construct_word lines (line_num + vert_inc) (char_idx + horiz_inc) horiz_inc vert_inc @@ word ^ String.make 1 c
        | exception _ -> word
      )
      | None -> word
      | exception _ -> word
    else 
      word
  in

  let find_all_matches lines line_num char_idx =
    Array.fold_left (fun num_found dir -> num_found + 
      if String.equal "XMAS" @@ construct_word lines line_num char_idx (fst dir) (snd dir) "" 
        then 1 
        else 0)
      0 part_1_dirs
  in

  let rec process line line_num idx num_found = 
    match line.[idx] with
    (* Further process, find the full word *)
    | 'X' -> num_found + find_all_matches lines line_num idx + process line line_num (idx + 1) num_found
    | c -> process line line_num (idx + 1) num_found
    | exception Invalid_argument _ -> num_found
  in

  snd @@ List.fold_left (fun (line_num, num_found) line -> line_num + 1, process line line_num 0 0 + num_found) (0, 0) lines

let part_2_dirs = [| 
  1, 1; 1, -1; (* right-down and right-up *)
|]

let part_2 lines =
  (* Limits to a length of four anyway *)
  let construct_word lines line_num char_idx dir =
    (* The character at `line_num, char_idx` will be 'A' *)
    String.make 1 (
      match List.nth_opt lines (line_num + snd dir) with
      | Some line -> (
        match line.[char_idx + fst dir] with
        | c -> c
        | exception _ -> 'p'
      )
      | None -> 'p'
      | exception _ -> 'p'
    )
    ^ "A" ^
    String.make 1 (
      match List.nth_opt lines (line_num - snd dir) with
      | Some line -> (
        match line.[char_idx - fst dir] with
        | c -> c
        | exception _ -> 'p'
      )
      | None -> 'p'
      | exception _ -> 'p'
    )
  in

  let find_x_mas lines line_num char_idx =
    if (
    Array.fold_left (fun num_found dir ->
      let word = construct_word lines line_num char_idx dir in
      (* let () = print_endline word in *)
      num_found + 
      if String.equal "MAS" word || String.equal "SAM" word
        then 1 
        else 0)
      0 part_2_dirs
    ) = 2 then 1 else 0

  in

  let rec process line line_num idx num_found = 
    match line.[idx] with
    (* Further process, find the full word *)
    | 'A' -> num_found + 
      find_x_mas lines line_num idx + 
      process line line_num (idx + 1) num_found
    | c -> process line line_num (idx + 1) num_found
    | exception Invalid_argument _ -> num_found
  in

  snd @@ List.fold_left (fun (line_num, num_found) line -> line_num + 1, process line line_num 0 0 + num_found) (0, 0) lines


let main () =
  let file = "../inputs/04.txt" in
  (* let file = "../inputs/04_example1.txt" in *)
  (* let file = "../inputs/04_example2.txt" in *)
  let lines = read_lines (open_in file) [] in
  part_1 lines |> string_of_int |> print_endline ;
  part_2 lines |> string_of_int |> print_endline ;;

  (* List.iter (print_endline) lines ;; *)

main ()