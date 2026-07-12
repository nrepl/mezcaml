let read_port file =
  match open_in file with
    | exception Sys_error _ -> None
    | ic ->
        let line = try input_line ic with End_of_file -> "" in
        close_in_noerr ic;
        int_of_string_opt (String.trim line)

let find ?from () =
  let rec walk dir =
    let candidate = Filename.concat dir ".nrepl-port" in
    if Sys.file_exists candidate then
      read_port candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else walk parent
  in
  walk (match from with Some dir -> dir | None -> Sys.getcwd ())
