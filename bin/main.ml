let host = ref "127.0.0.1"
let port = ref 0
let expr = ref None
let load = ref None

let args = [
  "--host", Arg.Set_string host,
  "<host> Host to connect to (default: 127.0.0.1)";
  "--port", Arg.Set_int port,
  "<port> Port to connect to (default: discovered via .nrepl-port)";
  "-p", Arg.Set_int port,
  "<port> Same as --port";
  "-e", Arg.String (fun code -> expr := Some code),
  "<code> Evaluate <code>, print the result and exit";
  "--load", Arg.String (fun file -> load := Some file),
  "<file> Send <file> to the server via load-file and exit";
]

let usage = "Usage: mezcaml [options]\n\nA minimal nREPL client.\n\nOptions:"

let die fmt = Printf.ksprintf (fun msg -> prerr_endline msg; exit 1) fmt

(* Print one response message the way a terminal user expects:
   evaluation output as it was written, values on their own line. *)
let print_response msg =
  (match Mezcaml.Bencode.get_string msg "out" with
     | Some out -> print_string out
     | None -> ());
  (match Mezcaml.Bencode.get_string msg "err" with
     | Some err -> prerr_string err
     | None -> ());
  (match Mezcaml.Bencode.get_string msg "value" with
     | Some value ->
         print_string value;
         if value = "" || value.[String.length value - 1] <> '\n' then
           print_newline ()
     | None -> ());
  flush stdout;
  flush stderr

let server_description conn =
  let versions =
    List.find_map
      (fun msg -> Mezcaml.Bencode.get msg "versions")
      (Mezcaml.Client.describe conn)
  in
  match versions with
    | Some (Mezcaml.Bencode.Dict entries) ->
        entries
        |> List.filter_map
             (fun (name, v) ->
                match Mezcaml.Bencode.get_string v "version-string" with
                  | Some version -> Some (name ^ " " ^ version)
                  | None -> None)
        |> String.concat ", "
    | _ -> "unknown server"

let repl conn =
  Printf.printf "mezcaml, a minimal nREPL client. Ctrl-D to quit.\n";
  Printf.printf "Connected to %s:%d (%s)\n" !host !port (server_description conn);
  let rec loop () =
    print_string "mezcaml> ";
    flush stdout;
    match read_line () with
      | exception End_of_file ->
          print_newline ();
          ignore (Mezcaml.Client.close_session conn : Mezcaml.Bencode.t list)
      | "" ->
          loop ()
      | line ->
          ignore (Mezcaml.Client.eval conn ~f:print_response line
                  : Mezcaml.Bencode.t list);
          loop ()
  in
  loop ()

let () =
  Arg.parse args
    (fun anon -> die "unexpected argument %S" anon)
    usage;
  if !port = 0 then begin
    match Mezcaml.Port_file.find () with
      | Some p -> port := p
      | None -> die "no port given and no .nrepl-port file found"
  end;
  let conn =
    try Mezcaml.Client.connect ~host:!host ~port:!port
    with Unix.Unix_error (error, _, _) ->
      die "cannot connect to %s:%d: %s" !host !port (Unix.error_message error)
  in
  ignore (Mezcaml.Client.clone conn : string);
  (match !expr, !load with
     | Some code, _ ->
         let responses = Mezcaml.Client.eval conn ~f:print_response code in
         Mezcaml.Client.close conn;
         exit (if Mezcaml.Client.eval_error responses then 1 else 0)
     | None, Some file ->
         let contents =
           try
             let ic = open_in_bin file in
             let contents = really_input_string ic (in_channel_length ic) in
             close_in ic;
             contents
           with Sys_error msg -> die "cannot read %s: %s" file msg
         in
         let responses =
           Mezcaml.Client.load_file conn ~f:print_response
             ~file_name:(Filename.basename file) contents
         in
         Mezcaml.Client.close conn;
         exit (if Mezcaml.Client.eval_error responses then 1 else 0)
     | None, None ->
         repl conn;
         Mezcaml.Client.close conn)
