let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  (* List.rev to get the lines in order *)
  with End_of_file -> List.rev l
;;

let part_1 lines = 
  let rules, updates = List.partition_map (fun line -> 
    match String.index_opt line '|' with
    | Some _ -> Left line
    | None -> Right (String.split_on_char ',' line)
    ) @@ List.filter (fun line -> not @@ String.equal "" line) lines
  in

  let ordering = List.fold_left (fun rules_map rule_str -> 
    match String.split_on_char '|' rule_str with
    | head :: tail :: [] -> (
      match List.assoc_opt head rules_map with
      | Some c -> (head, tail :: c) :: rules_map
      | None -> (head, tail :: []) :: rules_map
      )
    | _ -> rules_map
    
    ) [] rules
  in

  let compare i j = 
    if (
      match List.assoc_opt i ordering with
      | Some assoc_list -> List.mem j assoc_list
      | None -> false
    ) then -1
      else 
    if (
      match List.assoc_opt j ordering with
      | Some assoc_list -> List.mem i assoc_list
      | None -> false
    ) then 1
      else 0
  in

  List.fold_left (+) 0 (
    List.map (fun correct_update -> List.nth correct_update (List.length correct_update / 2) |> int_of_string) @@
      (List.filter (fun update -> update = List.stable_sort compare update) updates)
  )

let part_2 lines =
  let rules, updates = List.partition_map (fun line -> 
    match String.index_opt line '|' with
    | Some _ -> Left line
    | None -> Right (String.split_on_char ',' line)
    ) @@ List.filter (fun line -> not @@ String.equal "" line) lines
  in

  let ordering = List.fold_left (fun rules_map rule_str -> 
    match String.split_on_char '|' rule_str with
    | head :: tail :: [] -> (
      match List.assoc_opt head rules_map with
      | Some c -> (head, tail :: c) :: rules_map
      | None -> (head, tail :: []) :: rules_map
      )
    | _ -> rules_map
    
    ) [] rules
  in

  let compare i j = 
    if (
      match List.assoc_opt i ordering with
      | Some assoc_list -> List.mem j assoc_list
      | None -> false
    ) then -1
      else 
    if (
      match List.assoc_opt j ordering with
      | Some assoc_list -> List.mem i assoc_list
      | None -> false
    ) then 1
      else 0
  in

  List.fold_left (+) 0 (
    List.map (fun incorrect_update -> 
      let correct_order = List.stable_sort compare incorrect_update in
      List.nth correct_order (List.length correct_order / 2) |> int_of_string
      ) @@
      (List.filter (fun update -> update <> List.stable_sort compare update) updates)
  )

let main () =
  let file = "../inputs/05.txt" in
  (* let file = "../inputs/05_example.txt" in *)
  let lines = read_lines (open_in file) [] in

  (* List.mem "" lines |> string_of_bool |> print_endline;; *)
  (* List.iter print_string lines ;; *)

  (* List.mem  *)

  part_1 lines |> string_of_int |> print_endline ;
  part_2 lines |> string_of_int |> print_endline ;;

  (* List.iter (print_endline) lines ;; *)

main ()