let ( ** ) x n = let rec pow x n acc = match n with
    | 0 -> acc
    | _ -> pow x (n-1) (acc * x)
    in pow x n 1
    
let sum_range a b = (b * succ b) / 2 - (a * pred a) / 2
    
let read_file file = In_channel.with_open_text file In_channel.input_all

let list_to_pair l = match l with
    | first :: second :: [] -> (first, second)
    | _ -> raise (Invalid_argument "Input list must have exactly 2 elements.")
    
let parse_input s = String.split_on_char ',' s |> List.map (String.split_on_char '-') |> List.map list_to_pair

let range a b = List.init (b - a) (( + ) a)

let p1_sum_invalids_in_range (first, last) = let n1, l1, n2, l2 = int_of_string first, String.length first, int_of_string last, String.length last in
    if l1 == l2 && l1 mod 2 == 1
    then 0
    else let half_l1 = (l1 + 1) / 2 and half_l2 = (l2 / 2) + 1 in
        range half_l1 half_l2 |> List.map (fun half_len ->
            let fh1, fh2 = if l1 - half_len > 0 then int_of_string (String.sub first 0 (l1 - half_len)), int_of_string (String.sub first (l1 - half_len) half_len) else 0, n1
            and lh1, lh2 = if l2 - half_len > 0 then int_of_string (String.sub last 0 (l2 - half_len)), int_of_string (String.sub last (l2 - half_len) half_len) else 0, n2
            in
            sum_range 
                (max
                    (10 ** pred half_len)
                    (fh1 + (if fh2 > fh1 then 1 else 0))
                ) 
                (min
                    (10 ** half_len - 1)
                    (lh1 - (if lh2 < lh1 then 1 else 0))
                )
            * (10 ** half_len + 1)
        ) |> List.fold_left ( + ) 0

let part1 infile = read_file infile |> String.trim |> parse_input |> List.map p1_sum_invalids_in_range |> List.fold_left ( + ) 0
let part2 _infile = 0
