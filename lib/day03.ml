let read_lines file = In_channel.with_open_text file In_channel.input_lines

let take_first_two seq = match seq() with
    | Seq.Nil -> raise (Invalid_argument "Argument must have at least 2 elements")
    | Seq.Cons(e1, seq) -> match seq() with
        | Seq.Nil -> raise (Invalid_argument "Argument must have at least 2 elements")
        | Seq.Cons(e2, seq) -> (e1, e2, seq)

let digit_to_int digit = match digit with
    | '0'..'9' -> int_of_char digit - int_of_char '0'
    | _ -> raise (Invalid_argument "Argument must be a digit from 0 to 9")

let rec append e l = match l with
    | first :: rest -> first :: (append e rest)
    | [] -> [e]

let rec shift_digits e l = match l with
    | first :: second :: rest -> if second > first then append e (second :: rest) else first :: (shift_digits e (second :: rest))
    | first :: [] -> [max first e]
    | [] -> raise (Invalid_argument "Input list must not be empty")

let rec first_n n seq = match n with
    | 0 -> ([], seq)
    | _ -> match seq() with
        | Seq.Nil -> raise (Invalid_argument "Not enough elements in sequence")
        | Seq.Cons (e, seq) -> let (l, s) = (first_n (n - 1) seq) in (e :: l, s)

let find_n_max n seq =
    let rec aux l seq = match seq() with
        | Seq.Nil -> l
        | Seq.Cons (e, seq) -> aux (shift_digits e l) seq
    in let l, s = first_n n seq in
    aux l s

let list_to_joltage l = List.fold_left (fun acc n -> acc * 10 + n) 0 l

let find_joltage n str = if String.length str < n then raise (Invalid_argument "str argument must have at least n characters") else
    String.to_seq str |> Seq.map digit_to_int |> find_n_max n |> list_to_joltage

let part1 infile = read_lines infile |> List.map (find_joltage 2) |> List.fold_left ( + ) 0
let part2 infile = read_lines infile |> List.map (find_joltage 12) |> List.fold_left ( + ) 0
