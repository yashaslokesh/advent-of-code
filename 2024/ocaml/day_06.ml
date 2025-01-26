let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  (* List.rev to get the lines in order *)
  with End_of_file -> List.rev l
;;

let add_to_map_list_value assoc_list key new_value =
  match List.assoc_opt key assoc_list with
  | Some assoc -> (key, (new_value :: assoc)) :: assoc_list
  | None -> (key, [new_value]) :: assoc_list

(* Direction will be change in line_idx, change in char_idx *)
(* (-1, 0) -> (0, 1) -> (1, 0) -> (0, -1) *)
let rotate_90 (line_inc, char_inc) = char_inc, -1 * line_inc

let add_to_set x l =
  if List.mem x l then l else x :: l

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

  let rec find_potential line_idx char_idx direction =
    match List.nth lines (line_idx + fst direction) with
    | line -> (
      (* Printf.printf "%d, %d\n" line_idx char_idx ; *)
      match line.[char_idx + snd direction] with
      | '#' -> true
      | c -> find_potential (line_idx + fst direction) (char_idx + snd direction) direction
      | exception _ -> false
    )
    | exception _ -> false
  in

  (* Direction will be change in line_idx, change in char_idx *)
  (* (-1, 0) -> (0, 1) -> (1, 0) -> (0, -1) *)
  let rec traverse line_idx char_idx direction visited potential =
    match List.nth lines (line_idx + fst direction) with
    | line -> (
      let next_direction = rotate_90 direction in
      match line.[char_idx + snd direction] with
      | '#' -> 
        traverse (line_idx + fst next_direction) (char_idx + snd next_direction) next_direction 
          (add_to_set (line_idx + fst next_direction, char_idx + snd next_direction) visited) @@
          potential
      | c -> traverse (line_idx + fst direction) (char_idx + snd direction) direction 
          (add_to_set (line_idx + fst direction, char_idx + snd direction) visited)
          (if find_potential line_idx char_idx next_direction then
            add_to_set (line_idx + fst direction, char_idx + snd direction) potential else potential)
            (* (line_idx + fst direction, char_idx + snd direction) :: potential else potential) *)
      | exception _ -> visited, potential
    )
    | exception _ -> visited, potential
  in

  let (visited, potential) = traverse line_idx char_idx (-1, 0) [(line_idx, char_idx)] [] in
  
  let rec detect_cycle line_idx char_idx direction obstacle_coord visited_with_direction = 
    let next_line_idx, next_char_idx = line_idx + fst direction, char_idx + snd direction in
    let obstcl_line_idx, obstcl_char_idx = obstacle_coord in

    match List.nth lines next_line_idx with
    | line -> (
      let next_direction = rotate_90 direction in
      let rotated_next_line_idx, rotated_next_char_idx = line_idx + fst next_direction, char_idx + snd next_direction in
      match line.[next_char_idx] with
      | '#' -> (
        detect_cycle rotated_next_line_idx rotated_next_char_idx next_direction obstacle_coord 
        (add_to_map_list_value visited_with_direction direction (line_idx, char_idx))
      )
      | c -> (
        if next_line_idx = obstcl_line_idx && next_char_idx = obstcl_char_idx
          then detect_cycle rotated_next_line_idx rotated_next_char_idx next_direction obstacle_coord 
              (add_to_map_list_value visited_with_direction direction (line_idx, char_idx))
        else 
          match List.assoc_opt direction visited_with_direction with
          | Some visited_coords -> if List.mem (line_idx, char_idx) visited_coords then true else
              detect_cycle next_line_idx next_char_idx direction obstacle_coord (add_to_map_list_value visited_with_direction direction (line_idx, char_idx))
          | None -> detect_cycle next_line_idx next_char_idx direction obstacle_coord (add_to_map_list_value visited_with_direction direction (line_idx, char_idx))
      )

      | exception _ -> false
    )
    | exception _ -> false
  in

  snd @@ List.fold_left (fun (i, sum) obstacle_coord -> 
    print_int i ; print_newline () ;
    i + 1, sum + if detect_cycle line_idx char_idx (-1, 0) obstacle_coord [] then 1 else 0
  ) (0, 0) potential

let main () =
  let file = "../inputs/06.txt" in
  (* let file = "../inputs/06_example.txt" in *)
  let lines = read_lines (open_in file) [] in
  part_1 lines |> string_of_int |> print_endline ;
  part_2 lines |> string_of_int |> print_endline ;;

main ()