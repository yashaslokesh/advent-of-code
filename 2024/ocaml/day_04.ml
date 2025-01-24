let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  (* List.rev to get the lines in order *)
  with End_of_file -> List.rev l
;;

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
  else 
    word

let dirs = [| 
  (1,0); (-1, 0); (* Horizontal *)
  0, 1; 0, -1; (* Vertical *)
  1, 1; 1, -1; (* left-down and left-up *)
  -1, 1; -1, -1; (* right-down and right-up *)
  |]

let find_all_matches lines line_num char_idx =

  Array.fold_left (fun num_found dir -> num_found + 
    match construct_word lines line_num char_idx (fst dir) (snd dir) "" with
    | "XMAS" -> 1
    | _ | exception _ -> 0
    ) 0 dirs

let part_1 lines = 
  let rec process line line_num idx num_found = 
    match line.[idx] with
    (* Further process, find the full word *)
    | 'X' -> num_found + find_all_matches lines line_num idx + process line line_num (idx + 1) num_found
    | c -> process line line_num (idx + 1) num_found
    | exception Invalid_argument _ -> num_found
  in

  snd @@ List.fold_left (fun (line_num, num_found) line -> line_num + 1, process line line_num 0 0 + num_found) (0, 0) lines

let part_2 lines =
  0


let main () =
  let file = "../inputs/04.txt" in
  (* let file = "../inputs/04_example1.txt" in *)
  (* let file = "../inputs/04_example2.txt" in *)
  let lines = read_lines (open_in file) [] in
  part_1 lines |> string_of_int |> print_endline ;
  part_2 lines |> string_of_int |> print_endline ;;

  (* List.iter (print_endline) lines ;; *)

main ()