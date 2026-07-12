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

(* +-----------------------------------------------------------------+
   | Presentation                                                    |
   +-----------------------------------------------------------------+ *)

(* Colors are applied only when the stream is a terminal, NO_COLOR is
   unset and TERM isn't "dumb". *)
let want_color fd =
  Unix.isatty fd
  && Sys.getenv_opt "NO_COLOR" = None
  && Sys.getenv_opt "TERM" <> Some "dumb"

let color_stdout = lazy (want_color Unix.stdout)
let color_stderr = lazy (want_color Unix.stderr)

let style enabled code text =
  if Lazy.force enabled then
    Printf.sprintf "\027[%sm%s\027[0m" code text
  else
    text

let value_style text = style color_stdout "32" text
let error_style text = style color_stderr "31" text

(* Some servers (utop's multi-phrase evals, for one) send values with
   leading newlines; they only add stray blank lines on a terminal. *)
let strip_leading_newlines s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && s.[!i] = '\n' do incr i done;
  if !i = 0 then s else String.sub s !i (n - !i)

(* Print one response message the way a terminal user expects:
   evaluation output as it was written, errors in red on stderr,
   values in green on their own line. *)
let print_response msg =
  (match Mezcaml.Bencode.get_string msg "out" with
     | Some out -> print_string out
     | None -> ());
  (match Mezcaml.Bencode.get_string msg "err" with
     | Some err -> prerr_string (error_style err)
     | None -> ());
  (match Mezcaml.Bencode.get_string msg "value" with
     | Some value ->
         let value = strip_leading_newlines value in
         print_string (value_style value);
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

(* +-----------------------------------------------------------------+
   | Interactive REPL                                                |
   +-----------------------------------------------------------------+ *)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let history_file () =
  let state_dir =
    match Sys.getenv_opt "XDG_STATE_HOME" with
      | Some dir when dir <> "" -> Some dir
      | _ ->
          match Sys.getenv_opt "HOME" with
            | Some home -> Some (Filename.concat home ".local/state")
            | None -> None
  in
  match state_dir with
    | Some dir -> Some (Filename.concat (Filename.concat dir "mezcaml") "history")
    | None -> None

let setup_line_editor conn =
  LNoise.set_multiline true;
  LNoise.catch_break true;
  ignore (LNoise.history_set ~max_length:1000 : (unit, string) result);
  let history = history_file () in
  (match history with
     | Some file ->
         (try mkdir_p (Filename.dirname file) with Unix.Unix_error _ -> ());
         ignore (LNoise.history_load ~filename:file : (unit, string) result)
     | None -> ());
  (* Tab completion, powered by the server. The server completes the
     whole line and returns full-line candidates, so they can be
     handed to linenoise as they come. A server hiccup must not take
     down the line editor. *)
  LNoise.set_completion_callback
    (fun line completions ->
       if String.trim line <> "" then
         match Mezcaml.Client.completions conn line with
           | candidates ->
               List.iter (LNoise.add_completion completions) candidates
           | exception _ -> ());
  history

let repl conn =
  Printf.printf "mezcaml, a minimal nREPL client. Ctrl-D to quit.\n";
  Printf.printf "Connected to %s:%d (%s)\n" !host !port (server_description conn);
  let history = setup_line_editor conn in
  let remember line =
    ignore (LNoise.history_add line : (unit, string) result);
    match history with
      | Some file ->
          ignore (LNoise.history_save ~filename:file : (unit, string) result)
      | None -> ()
  in
  let rec loop () =
    match LNoise.linenoise "mezcaml> " with
      | exception Sys.Break ->
          (* Ctrl-C cancels the current line, not the session. *)
          print_newline ();
          loop ()
      | None ->
          print_newline ();
          ignore (Mezcaml.Client.close_session conn : Mezcaml.Bencode.t list)
      | Some "" ->
          loop ()
      | Some line ->
          remember line;
          ignore (Mezcaml.Client.eval conn ~f:print_response line
                  : Mezcaml.Bencode.t list);
          loop ()
  in
  loop ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

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
