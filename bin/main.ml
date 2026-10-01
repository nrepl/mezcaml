let host = ref "127.0.0.1"
let port = ref 0
let expr = ref None
let load = ref None
let timeout = ref 0.0
let color_mode = ref "auto"

let die fmt = Printf.ksprintf (fun msg -> prerr_endline msg; exit 1) fmt

let version () =
  match Build_info.V1.version () with
    | Some v -> Build_info.V1.Version.to_string v
    | None -> "dev"

let set_once what cell value =
  if !cell <> None then die "%s can only be given once" what;
  cell := Some value

let args = [
  "--host", Arg.Set_string host,
  "<host> Host to connect to (default: 127.0.0.1)";
  "--port", Arg.Set_int port,
  "<port> Port to connect to (default: discovered via .nrepl-port)";
  "-p", Arg.Set_int port,
  "<port> Same as --port";
  "-e", Arg.String (set_once "-e" expr),
  "<code> Evaluate <code>, print the result and exit";
  "--load", Arg.String (set_once "--load" load),
  "<file> Send <file> to the server via load-file and exit";
  "--timeout", Arg.Set_float timeout,
  "<seconds> Give up when connecting or waiting for a response takes longer";
  "--color", Arg.Symbol (["auto"; "always"; "never"], (fun mode -> color_mode := mode)),
  " When to color output (default: auto)";
  "--version", Arg.Unit (fun () -> Printf.printf "mezcaml %s\n" (version ()); exit 0),
  " Print version and exit";
]

let usage = "Usage: mezcaml [options]\n\nA minimal nREPL client.\n\nOptions:"

(* +-----------------------------------------------------------------+
   | Presentation                                                    |
   +-----------------------------------------------------------------+ *)

(* With --color auto (the default), colors are applied only when the
   stream is a terminal, NO_COLOR is unset and TERM isn't "dumb". *)
let want_color fd =
  match !color_mode with
    | "always" -> true
    | "never" -> false
    | _ ->
        Unix.isatty fd
        && Sys.getenv_opt "NO_COLOR" = None
        && Sys.getenv_opt "TERM" <> Some "dumb"

(* Forced only once the command line has been parsed. *)
let output =
  lazy
    (Output.create ~out_color:(want_color Unix.stdout)
       ~err_color:(want_color Unix.stderr) ())

let output () = Lazy.force output

let server_description versions =
  if versions = [] then "unknown server"
  else
    versions
    |> List.map (fun (name, v) -> if v = "" then name else name ^ " " ^ v)
    |> String.concat ", "

(* +-----------------------------------------------------------------+
   | Evaluation                                                      |
   +-----------------------------------------------------------------+ *)

(* The namespace evaluations last reported. Completion requests send
   it along, since some servers (jank) won't complete without one. *)
let current_ns = ref None

let on_response msg =
  Output.print_response (output ()) msg;
  match Mezcaml.Bencode.get_string msg "ns" with
    | Some ns -> current_ns := Some ns
    | None -> ()

(* Print an evaluation's responses as they arrive. Meanwhile Ctrl-C
   asks the server to interrupt it instead of killing mezcaml, which
   would leave the evaluation running and the session open. A second
   Ctrl-C does kill mezcaml, for servers that can't interrupt.

   [read_input] answers the server's requests for stdin. *)
let evaluate ~read_input evaluation =
  let output = output () in
  Evaluation.with_sigint evaluation
    ~on_signal:(fun () -> Output.ctrl_c_echoed output)
    (fun () ->
       Evaluation.run evaluation ~on_response ~report:(Output.report output)
         ~read_input:(fun () -> read_input evaluation))

(* With -e and --load the evaluation reads mezcaml's own stdin. *)
let forward_stdin =
  let read = Evaluation.input_reader Unix.stdin in
  fun _ -> read ()

(* nREPL and bb keep a session around after its client disconnects,
   so mezcaml closes its own before it goes. That's housekeeping: it
   mustn't hang on a server that never answers, and a server that
   hangs up instead of answering doesn't make the run a failure. *)
let close_session conn =
  try
    ignore (Mezcaml.Client.close_session ~timeout:2.0 conn
            : Mezcaml.Bencode.t list)
  with End_of_file | Sys_blocked_io | Sys_error _ | Unix.Unix_error _
     | Failure _ -> ()

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

(* linenoise has the terminal in raw mode until it returns, so strays
   that come in meanwhile (during Tab completion) wait until then. *)
let linenoise prompt =
  Fun.protect
    (fun () -> LNoise.linenoise prompt)
    ~finally:(fun () -> Output.release (output ()))

(* A line for the evaluation's stdin, read at a prompt of its own.
   Ctrl-D sends end of input. *)
let prompt_for_input evaluation =
  Output.fresh_line (output ());
  match linenoise "stdin> " with
    | Some line -> Some (line ^ "\n")
    (* Without a terminal to put in raw mode (TERM=dumb, stdin a
       pipe), linenoise reads with fgets, which a SIGINT cuts short as
       if the input had ended. *)
    | None when Evaluation.ctrl_c_pending evaluation -> None
    | None -> Some ""
    | exception Sys.Break -> Evaluation.press_ctrl_c evaluation; None

let setup_line_editor conn ~ops =
  LNoise.set_multiline true;
  LNoise.catch_break true;
  ignore (LNoise.history_set ~max_length:1000 : (unit, string) result);
  let history = history_file () in
  (match history with
     | Some file ->
         (try mkdir_p (Filename.dirname file) with Unix.Unix_error _ -> ());
         ignore (LNoise.history_load ~filename:file : (unit, string) result)
     | None -> ());
  (* Tab completion: ask the server about the trailing token (see
     Repl_input), with whichever completion op it has. A server hiccup
     must not take down the line editor. *)
  let op = Mezcaml.Client.completion_op ops in
  LNoise.set_completion_callback
    (fun line completions ->
       let head, token = Repl_input.split_for_completion line in
       if token <> "" && op <> None then
         match
           Output.hold (output ()) (fun () ->
             Mezcaml.Client.completions conn ?op ?ns:!current_ns token)
         with
           | candidates ->
               List.iter
                 (fun candidate ->
                    LNoise.add_completion completions (head ^ candidate))
                 candidates
           | exception _ -> ());
  history

let repl conn =
  let description = Mezcaml.Client.describe conn in
  let versions = Mezcaml.Client.versions description in
  (* The banner is for interactive users; keep scripted (piped) runs
     clean. *)
  if Unix.isatty Unix.stdin then begin
    Printf.printf "mezcaml %s, a minimal nREPL client. Ctrl-D to quit.\n" (version ());
    Printf.printf "Connected to %s:%d (%s)\n" !host !port (server_description versions)
  end;
  let dialect = Repl_input.dialect_of_versions (List.map fst versions) in
  let history =
    setup_line_editor conn ~ops:(Mezcaml.Client.ops description)
  in
  let remember line =
    ignore (LNoise.history_add line : (unit, string) result)
  in
  (* Saved once per form rather than per line: linenoise rewrites
     the whole file on every save, which adds up when a big form is
     pasted in. *)
  let save_history () =
    match history with
      | Some file ->
          ignore (LNoise.history_save ~filename:file : (unit, string) result)
      | None -> ()
  in
  (* Keep reading continuation lines until the input amounts to a
     whole form. Each physical line is remembered separately because
     the history file is line-based. *)
  let rec read_form acc =
    let prompt = if acc = [] then "mezcaml> " else "    ...> " in
    match linenoise prompt with
      | exception Sys.Break ->
          (* Ctrl-C cancels the input being edited, not the session. *)
          Output.end_line (output ());
          read_form []
      | None -> `Eof
      | Some "" when acc = [] -> read_form []
      | Some line ->
          (* Blank continuation lines aren't worth a history entry. *)
          if line <> "" then remember line;
          let acc = line :: acc in
          let form = String.concat "\n" (List.rev acc) in
          if Repl_input.complete dialect form then `Form form
          else read_form acc
  in
  let rec loop () =
    match read_form [] with
      | `Eof ->
          save_history ();
          Output.end_line (output ());
          close_session conn
      | `Form form ->
          save_history ();
          ignore (evaluate ~read_input:prompt_for_input
                    (Evaluation.eval conn form)
                  : Mezcaml.Bencode.t list);
          loop ()
  in
  loop ()

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

(* A timed-out read surfaces as Sys_error/Unix_error from inside the
   channel layer; an abruptly closed connection as End_of_file; a
   malformed response (or not an nREPL server at all) as Failure.
   Turn them into a proper error message instead of a backtrace. *)
let guard_server_errors f =
  try f () with
    | End_of_file ->
        die "the server closed the connection"
    | Failure msg ->
        die "unexpected response from the server: %s" msg
    | Sys_blocked_io ->
        die "timed out waiting for the server (--timeout %g)" !timeout
    | Sys_error msg ->
        die "lost the server (timeout or connection error): %s" msg
    | Unix.Unix_error (error, _, _) ->
        die "lost the server (timeout or connection error): %s"
          (Unix.error_message error)

let () =
  (* A write to a server that has gone away should fail with EPIPE,
     which the guards report, rather than kill mezcaml without a
     word. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Arg.parse args
    (fun anon -> die "unexpected argument %S" anon)
    usage;
  if !expr <> None && !load <> None then
    die "-e and --load cannot be combined";
  if !port = 0 then begin
    match Mezcaml.Port_file.find () with
      | Some p -> port := p
      | None -> die "no port given and no .nrepl-port file found"
  end;
  let timeout = if !timeout > 0.0 then Some !timeout else None in
  let conn =
    try
      Mezcaml.Client.connect ?timeout ~other:(Output.stray (output ()))
        ~host:!host ~port:!port ()
    with
      | Unix.Unix_error (error, _, _) ->
          die "cannot connect to %s:%d: %s" !host !port
            (Unix.error_message error)
      | Failure msg -> die "cannot connect to %s:%d: %s" !host !port msg
  in
  guard_server_errors (fun () ->
    ignore (Mezcaml.Client.clone conn : string));
  (match !expr, !load with
     | Some code, _ ->
         let responses =
           guard_server_errors (fun () ->
             evaluate ~read_input:forward_stdin (Evaluation.eval conn code))
         in
         close_session conn;
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
           guard_server_errors (fun () ->
             evaluate ~read_input:forward_stdin
               (Evaluation.load_file conn ~file_name:(Filename.basename file)
                  contents))
         in
         close_session conn;
         Mezcaml.Client.close conn;
         exit (if Mezcaml.Client.eval_error responses then 1 else 0)
     | None, None ->
         guard_server_errors (fun () -> repl conn);
         Mezcaml.Client.close conn)
