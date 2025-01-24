let rec read_lines ic l =
  try
      read_lines ic (input_line ic :: l)
  (* List.rev to get the lines in order *)
  with End_of_file -> List.rev l
;;

let part_1 lines = 
  0

let part_2 lines =
  0


let main () =
  let file = "../inputs/02.txt" in
  let lines = read_lines (open_in file) [] in
  part_1 lines |> string_of_int |> print_endline ;
  part_2 lines |> string_of_int |> print_endline ;;

  (* List.iter (print_endline) lines ;; *)

main ()