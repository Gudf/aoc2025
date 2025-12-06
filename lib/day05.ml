let parse_range r = String.split_on_char '-' r |> List.map int_of_string |> (function s :: l :: [] -> (s, l) | _ -> raise (Invalid_argument "Not a valid range"))

let parse_ranges lines = let rec aux lines ranges = match lines with
        | [] -> ranges, []
        | "" :: rest -> ranges, rest
        | range_str :: rest -> aux rest (parse_range range_str :: ranges)
    in aux lines []

let parse_input lines =
    let ranges, remaining = parse_ranges lines in
    ranges, List.map int_of_string remaining

let in_range id (range_start, range_end) = range_start <= id && id <= range_end

let in_any_range ranges id = List.exists (in_range id) ranges

let merge_sorted_ranges ranges =
    let rec aux current remaining merged = match remaining with
        | [] -> current :: merged
        | (s, e) :: remaining -> if s > snd current
            then aux (s, e) remaining (current :: merged)
            else aux (fst current, max e (snd current)) remaining merged
    in aux (List.hd ranges) (List.tl ranges) []

let part1 infile = Io.read_lines infile |> parse_input |> fun (ranges, ids) -> List.filter (in_any_range ranges) ids |> List.length
let part2 infile = Io.read_lines infile |> parse_ranges |> fst |> List.sort compare |> merge_sorted_ranges |> List.map (fun (a, b) -> b - a + 1) |> List.fold_left ( + ) 0
