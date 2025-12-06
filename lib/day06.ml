let parse_input_p1 lines = let rec aux previous lines =
    match lines with
        | [ops] -> List.map (function "+" -> (( + ), 0) | "*" -> (( * ), 1) | _ -> raise (Invalid_argument "Wrong operator")) ops, previous
        | numbers :: rest -> aux ((List.map int_of_string numbers)::previous) rest
        | _ -> raise (Invalid_argument "Empty list passed to parse_input")
    in lines |> List.map (String.split_on_char ' ') |> List.map (List.filter (fun s -> String.length s != 0)) |> aux []

let parse_input_p2 lines = List.map String.to_seq lines

let transpose l = let rec aux t l =
    match l with
        | [] -> [[]]
        | [] :: _ -> t
        | (_ :: _) :: _ -> aux ((List.map List.hd l) :: t) (List.map List.tl l)
    in List.rev (aux [] l)

let part1 infile = Io.read_lines infile |> parse_input_p1 |> fun (f, s) -> (f, transpose s) |> fun (f, s) -> (Seq.zip (List.to_seq f) (List.to_seq s)) |> Seq.map (fun ((op, acc), operands) -> (List.fold_left op acc operands)) |> Seq.fold_left ( + ) 0
