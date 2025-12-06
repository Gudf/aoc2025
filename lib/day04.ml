let line_to_int_arr line = String.to_seq line |> Seq.map (fun c -> if c == '@' then 1 else 0) |> Array.of_seq

let copy_mat mat = Array.map Array.copy mat

let sum_rows_3 mat = let dst = copy_mat mat in
    for i = 0 to Array.length mat - 1 do
        for j = 0 to Array.length mat.(0) - 1 do
            dst.(i).(j) <- (if j < 1 then 0 else mat.(i).(j - 1)) + mat.(i).(j) + (if j + 1 < Array.length mat.(0) then mat.(i).(j + 1) else 0)
        done
    done; dst

let sum_cols_3 mat = let dst = copy_mat mat in
    for j = 0 to Array.length mat.(0) - 1 do
        for i = 0 to Array.length mat - 1 do
            dst.(i).(j) <- (if i < 1 then 0 else mat.(i - 1).(j)) + mat.(i).(j) + (if i + 1 < Array.length mat then mat.(i + 1).(j) else 0)
        done
    done; dst

let diff mat2 mat1 =
    for j = 0 to Array.length mat1.(0) - 1 do
        for i = 0 to Array.length mat1 - 1 do
            mat1.(i).(j) <- mat1.(i).(j) - mat2.(i).(j)
        done
    done; mat1

let mask m mat = let num_rows = Array.length mat and num_cols = Array.length mat.(0) in
    let dst = Array.make_matrix num_rows num_cols None in
    for j = 0 to num_cols - 1 do
        for i = 0 to num_rows - 1 do
            dst.(i).(j) <- if m.(i).(j) > 0 then Some(mat.(i).(j)) else None
        done
    done; dst

let calc_adjacencies orig = let mat = copy_mat orig in
    let adj = mat |> sum_rows_3
        |> sum_cols_3
    in diff orig adj |> mask orig

let remove_accessible locations = let adj = calc_adjacencies locations and n = ref 0 in
    for j = 0 to Array.length locations.(0) - 1 do
        for i = 0 to Array.length locations - 1 do
            match adj.(i).(j) with
                | Some(v) -> if v < 4 then (n := !n + 1; locations.(i).(j) <- 0;)
                | _ -> ()
        done
    done; if !n > 0 then Some(!n, locations) else None



let part1 infile = Io.read_lines infile |> List.map line_to_int_arr |> Array.of_list |> calc_adjacencies |> Array.to_list |> List.map Array.to_list |> List.flatten |> List.filter (function Some(v) -> v < 4 | None -> false) |> List.length
let part2 infile = Io.read_lines infile |> List.map line_to_int_arr |> Array.of_list |> Seq.unfold remove_accessible |> Seq.fold_left ( + ) 0
