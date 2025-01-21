let rec read_lines ic l =
    try
        read_lines ic (input_line ic :: l)
    with End_of_file -> l
;;

let part_1 lines = 
    let diff_checker a b =
        let abs = a - b |> Int.abs in
        abs >= 1 && abs <= 3
    in

    let safe report = 
        let rec evaluate report_type nums = 
            match nums with
            | head :: second :: tail -> (&&)
                (match report_type with
                    | 0 -> true
                    | x -> if x * (second - head) > 0 then true else false
                )
                (diff_checker head second) && evaluate (second - head) (second :: tail)
            | head :: [] -> true
            | [] -> true
        in
        
        evaluate 0 @@ (List.map int_of_string @@ String.split_on_char ' ' report)
    in

    List.fold_left (fun acc a -> 
        acc + if safe a then 1 else 0
        ) 0 lines

let part_2 lines = 
    let diff_checker a b =
        let abs = a - b |> Int.abs in
        abs >= 1 && abs <= 3
    in

    let safe report = 
        let rec evaluate report_type nums = 
            match nums with
            | head :: second :: tail ->
                (match report_type with
                    | 0 -> true
                    | x -> if x * (second - head) > 0 then true else false
                )
                &&
                (diff_checker head second) 
                && 
                evaluate (second - head) (second :: tail)
            | head :: [] -> true
            | [] -> true
        in
        
        evaluate 0 @@ (List.map int_of_string @@ String.split_on_char ' ' report)
    in

    List.fold_left (fun acc a -> 
        acc + if safe a then 1 else 0
        ) 0 lines

let main () =
    let file = "../inputs/02.txt" in
    let lines = read_lines (open_in file) [] in
    part_1 lines |> string_of_int |> print_endline ;;
    (* let (left, right) = split_into_two_lists lines [] [] in *)

main ()
            

