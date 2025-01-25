let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  (* List.rev to get the lines in order *)
  with End_of_file -> List.rev l
;;

(* Direction will be change in line_idx, change in char_idx *)
(* (-1, 0) -> (0, 1) -> (1, 0) -> (0, -1) *)
let rotate_90 old_dir = snd old_dir, -1 * fst old_dir

let add_to_set x l =
  if List.mem x l then l else x :: l

(* let rotate_90 line_idx char_idx =
  int_of_float @@ float_of_int char_idx *. cos (Float.pi /. 2.) -. float_of_int line_idx *. sin (Float.pi /. 2.),
  int_of_float @@ float_of_int char_idx *. sin (Float.pi /. 2.) +. float_of_int line_idx *. cos (Float.pi /. 2.) *)

let part_1 lines = 
  let line_idx = match List.find_index (fun line -> 
      match String.index_opt line '^' with
      | Some idx -> true
      | None -> false
      ) lines with
    | Some idx -> idx
    | None -> invalid_arg "Couldn't find ^"
  in

  let char_idx = match String.index_opt (List.nth lines line_idx) '^' with
    | Some idx -> idx
    | None -> invalid_arg "Couldn't find ^"
  in

  (* Direction will be change in line_idx, change in char_idx *)
  (* (-1, 0) -> (0, 1) -> (1, 0) -> (0, -1) *)
  let rec traverse line_idx char_idx direction visited =
    match List.nth lines (line_idx + fst direction) with
    | line -> (
      match line.[char_idx + snd direction] with
      | '#' -> 
        let next_direction = rotate_90 direction in
        traverse (line_idx + fst next_direction) (char_idx + snd next_direction) next_direction 
          (add_to_set (line_idx + fst next_direction, char_idx + snd next_direction) visited)
      | c -> traverse (line_idx + fst direction) (char_idx + snd direction) direction 
          (add_to_set (line_idx + fst direction, char_idx + snd direction) visited)
      | exception _ -> visited
    )
    | exception _ -> visited
  in

  List.length @@ traverse line_idx char_idx (-1, 0) [(line_idx, char_idx)] 

let part_2 lines =
  0


let main () =
  let file = "../inputs/06.txt" in
  (* let file = "../inputs/06_example.txt" in *)
  let lines = read_lines (open_in file) [] in
  part_1 lines |> string_of_int |> print_endline ;;
  (* part_2 lines |> string_of_int |> print_endline ;; *)

  (* List.iter (print_endline) lines ;; *)

main ()