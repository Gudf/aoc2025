let read_lines file = In_channel.with_open_text file In_channel.input_lines

let take_first_two seq = match seq() with
    | Seq.Nil -> raise (Invalid_argument "Argument must have at least 2 elements")
    | Seq.Cons(e1, seq) -> match seq() with
        | Seq.Nil -> raise (Invalid_argument "Argument must have at least 2 elements")
        | Seq.Cons(e2, seq) -> (e1, e2, seq)

let digit_to_int digit = match digit with
    | '0'..'9' -> int_of_char digit - int_of_char '0'
    | _ -> raise (Invalid_argument "Argument must be a digit from 0 to 9")

let find_two_max seq = let rec aux highest next_highest seq = match seq() with
        | Seq.Cons (e, seq) -> aux (max next_highest highest) (if highest >= next_highest then max e next_highest else e) seq
        | Seq.Nil -> (highest, next_highest)
    in let first, second, seq2 = take_first_two seq
    in aux first second seq2

let pair_to_joltage (a, b) = a * 10 + b

let find_joltage str = if String.length str < 2 then raise (Invalid_argument "Argument must have 2 or more characters") else
    String.to_seq str |> Seq.map digit_to_int |> find_two_max |> pair_to_joltage

let part1 infile = read_lines infile |> List.map find_joltage |> List.fold_left ( + ) 0
let part2 _infile = 0
