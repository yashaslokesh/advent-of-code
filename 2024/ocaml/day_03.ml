let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  with End_of_file -> List.rev l
;;

let sep = ','
let mul_first_char = 'm'
let mul_string = "mul"

(* The parser for the `mul` command. This function should be called when `mul` is detected. *)
(* This will process the rest of the string and command. *)
let process_mul_command from_idx line =
  (* First character should be `(` *)
  match line.[from_idx + 3] with
  | '(' -> ( (* first char of the command, at from_idx + 3 *)
    match String.index_from_opt line (from_idx + 4) sep with
    (* If we found the comma, attempt to extract the string *)
    | Some comma_idx -> (
      match int_of_string_opt @@ String.sub line (from_idx + 4) (comma_idx - (from_idx + 4)) with
      (* Valid integer, now look for the second half of the `mul` command *)
      | Some left -> (
        match String.index_from_opt line (comma_idx + 1) ')' with
        | Some close_paren_idx -> (
          match int_of_string_opt @@ String.sub line (comma_idx + 1) (close_paren_idx - (comma_idx + 1)) with
          | Some right -> right * left
          | None -> 0
        )
        | None -> 0
      )
      | None -> 0
    )
    | None -> 0
  )
  | c -> 0
  | exception Invalid_argument _ -> 0
  

let part_1 lines = 
  (* from_idx will be the point at which we continue processing `line` *)
  (* Sum is the running sum just for this line *)
  let rec compute_mul_for_line line from_idx sum =
    (* Search for the first 'm' in the string, we continue processing from here *)
    match String.index_from_opt line from_idx mul_first_char with
      (* if the 'm' results in a "mul", then process *)
      (* compute_mul_for_line line (idx + 1) @@ sum + 1 *)
    | Some idx -> (
      match String.sub line idx 3 with
      | "mul" -> compute_mul_for_line line (idx + 1) (sum + process_mul_command idx line)
      | s -> compute_mul_for_line line (from_idx + 1) sum
      | exception Invalid_argument _ -> sum
      )
    | None -> sum
  in

  List.fold_left (fun sum line -> compute_mul_for_line line 0 0 + sum) 0 lines

let part_2 lines =
  (* from_idx will be the point at which we continue processing `line` *)
  (* Sum is the running sum just for this line *)
  let rec compute_mul_for_line line from_idx sum mul_enabled =
    (* Part two, we will actually just traverse one character at a time since we need to process *)
    (* multiple possible commands at once *)
    match line.[from_idx] with
    | 'm' -> if mul_enabled then (
      match String.sub line from_idx 3 with
      | "mul" -> compute_mul_for_line line (from_idx + 3) (sum + process_mul_command from_idx line) mul_enabled
      | s -> compute_mul_for_line line (from_idx + 1) sum mul_enabled
      | exception Invalid_argument _ -> sum, mul_enabled
    ) else compute_mul_for_line line (from_idx + 1) sum mul_enabled
    (* Only the 'd' branch can possibly alter mul_enabled *)
    | 'd' -> (
      match String.sub line from_idx 4 with
      (* Set `mul_enabled` to true henceforth *)
      | "do()" -> compute_mul_for_line line (from_idx + 4) sum true
      | s -> (
        match String.sub line from_idx 7 with
        | "don't()" -> compute_mul_for_line line (from_idx + 7) sum false
        | s -> compute_mul_for_line line (from_idx + 1) sum mul_enabled
        | exception Invalid_argument _ -> sum, mul_enabled
      )
      | exception Invalid_argument _ -> sum, mul_enabled
    )
    | c -> compute_mul_for_line line (from_idx + 1) sum mul_enabled
    | exception Invalid_argument _ -> sum, mul_enabled
  in


  fst @@ List.fold_left (fun (sum, mul_enabled) line -> 
    let res = compute_mul_for_line line 0 0 mul_enabled in
    fst res + sum, snd res
  ) (0, true) lines


let main () =
  (* let file = "../inputs/03_example.txt" in *)
  let file = "../inputs/03.txt" in
  let lines = read_lines (open_in file) [] in
  part_1 lines |> string_of_int |> print_endline ;
  part_2 lines |> string_of_int |> print_endline ;;

  (* List.iter (print_endline) lines ;; *)

main ()