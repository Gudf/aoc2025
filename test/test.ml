let run_test t = 
    let (summary, test_fn, inp, expected) = t in 
        let res = (test_fn inp) in
            if res == expected
                then Printf.printf "[✓] %s:\n    Expected %i, got %i\n" summary expected res
                else Printf.printf "[x] %s:\n    Expected %i, got %i\n" summary expected res;
            res == expected

let run_tests s = 
    let rec aux tests = match tests with
        | first :: rest -> run_test first :: aux rest
        | [] -> []
    in Printf.printf "Running suite %s:\n\n" (fst s); snd s |> List.rev |> aux |> (List.fold_left ( && ) true) |> not |> Bool.to_int
