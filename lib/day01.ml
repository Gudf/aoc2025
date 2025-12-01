let read_lines file = In_channel.with_open_text file In_channel.input_lines

let reduce init op l = 
    let rec aux l acc =
        match l with
            | first :: rest -> let v = op acc first in aux rest v
            | [] -> acc
    in aux l init
    
let rec accumulate init op l = match l with
    | first :: rest -> let v = op init first in v :: accumulate v op rest
    | [] -> []

let rec filter test l = match l with
    | first :: rest -> if test first then first :: (filter test rest) else (filter test rest)
    | [] -> []
    
let parse_entry e = match e.[0] with
    | 'R' -> int_of_string (String.sub e 1 (String.length e - 1))
    | 'L' -> - (int_of_string (String.sub e 1 (String.length e - 1)))
    | _ -> raise (Invalid_argument e)
    
let rec map f l = match l with
    | first :: rest -> (f first) :: (map f rest)
    | [] -> []
    
let length l =
    let rec aux l v = match l with
        | _ :: rest -> aux rest (v + 1)
        | [] -> v
    in aux l 0

let mod_add m a b = (((a + b) mod m) + m) mod m

let second (_, b) = b

let part1 infile = (read_lines infile) |> (map parse_entry) |> (accumulate 50 (mod_add 100)) |> (filter (fun x -> x == 0)) |> length
let part2 infile = (read_lines infile) |> (map parse_entry) |> (reduce(50, 0) (fun (a, n) b -> mod_add 100 a b, n + (if a + b > 0 then (a + b) / 100 else if a == 0 then abs b / 100 else abs (a + b) / 100 + 1))) |> second
