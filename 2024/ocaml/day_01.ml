open Stdlib.String

let file = "../inputs/01.txt" ;;

let rec read_lines ic l =
    try
        read_lines ic (input_line ic :: l)
    with End_of_file -> l
;;

let rec split_into_two_lists l left right = 
    match l with
    | s :: tail -> 
        let split = split_on_char ' ' s in
        split_into_two_lists tail 
            ((List.hd split |> int_of_string) :: left) 
            ((List.rev split |> List.hd |> int_of_string) :: right)
    | [] -> 
        List.sort Int.compare left, List.sort Int.compare right
;;

(* Part 1 solution *)
let compute_distances left right =
    List.fold_left2 
        (fun total_distance l r -> (+) total_distance ((-) l r |> Int.abs))
        0 left right

(* Part 2 solution *)
(* Could be more optimized by using Map or Hashtbl *)
let rec generate_num_count_list right assoc_list = 
    match right with
    | head :: tail -> 
        (match List.assoc_opt head assoc_list with
        | Some b ->  generate_num_count_list tail ((head, b + 1) :: assoc_list)
        | None -> generate_num_count_list tail ((head, 1) :: assoc_list))
    | [] -> assoc_list
;;

let compute_similiary left right = 
    let assoc_list = generate_num_count_list right [] in
    List.fold_left 
        (fun acc a -> match List.assoc_opt a assoc_list with
            | Some b -> (a * b) + acc
            | None -> acc)
        0 left

let main () =
    let lines = read_lines (open_in file) [] in
    let (left, right) = split_into_two_lists lines [] [] in

    compute_distances left right |> string_of_int |> print_endline ;
    compute_similiary left right |> string_of_int |> print_endline ;;

main ()



