let read_lines file = In_channel.with_open_text file In_channel.input_lines

let read_file file = In_channel.with_open_text file In_channel.input_all
