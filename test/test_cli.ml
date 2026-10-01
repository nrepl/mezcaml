(* mezcaml itself, run against a fake server. *)

open Fake_server

let mezcaml = "../bin/main.exe"

let read_file file =
  let ic = open_in_bin file in
  let text = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Sys.remove file;
  text

(* Run mezcaml with [args], [feed] writing its stdin (given mezcaml's
   pid, to send it signals); returns how it exited and what it
   printed. It gets a history file of its own and is killed if it's
   still running after ten seconds. *)
let run ?(feed = fun _ _ -> ()) args =
  let out_file = Filename.temp_file "mezcaml-cli" ".out" in
  let err_file = Filename.temp_file "mezcaml-cli" ".err" in
  let open_output file =
    Unix.openfile file [Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CLOEXEC] 0o600
  in
  let out = open_output out_file in
  let err = open_output err_file in
  let stdin_r, stdin_w = Unix.pipe ~cloexec:true () in
  let env =
    Array.append
      [| "XDG_STATE_HOME=" ^ Filename.get_temp_dir_name (); "NO_COLOR=1" |]
      (Unix.environment ())
  in
  let pid =
    Unix.create_process_env mezcaml (Array.of_list (mezcaml :: args)) env
      stdin_r out err
  in
  List.iter Unix.close [stdin_r; out; err];
  (try feed pid stdin_w with Unix.Unix_error _ -> ());
  Unix.close stdin_w;
  let rec wait tries =
    match Unix.waitpid [Unix.WNOHANG] pid with
      | 0, _ when tries > 0 -> Thread.delay 0.05; wait (tries - 1)
      | 0, _ ->
          Unix.kill pid Sys.sigkill;
          snd (Unix.waitpid [] pid)
      | _, status -> status
  in
  let status = wait 200 in
  (status, read_file out_file, read_file err_file)

let write text fd =
  ignore (Unix.write_substring fd text 0 (String.length text) : int)

(* A server that answers clone and eval, and [on_close] for close. *)
let server ~on_close request =
  match op_of request with
    | "clone" -> session_reply request
    | "close" -> on_close request
    | _ -> answer "3" request

let () =
  (* A server that never answers close holds up -e for no more than a
     couple of seconds, and doesn't fail it. *)
  let (outcome, out, _), _ =
    with_tcp_server (server ~on_close:(fun _ -> []))
      (fun port -> run ["-p"; string_of_int port; "-e"; "(+ 1 2)"])
  in
  assert (outcome = Unix.WEXITED 0);
  assert (out = "3\n");

  (* Nor does one that hangs up instead of answering make quitting the
     REPL a failure. *)
  let (outcome, out, err), requests =
    with_tcp_server (server ~on_close:(fun _ -> [hang_up]))
      (fun port ->
         run ["-p"; string_of_int port]
           ~feed:(fun _ fd -> write "(+ 1 2)\n" fd))
  in
  assert (outcome = Unix.WEXITED 0);
  assert (out = "3\n\n" && err = "");
  assert (List.map op_of requests = ["clone"; "describe"; "eval"; "close"]);

  (* Ctrl-C at the stdin prompt of a REPL that can't use the terminal
     (stdin a pipe, here) interrupts the evaluation, rather than send
     it the end of its input. *)
  let asked = ref false in
  let (outcome, _, _), requests =
    with_tcp_server
      (fun request ->
         match op_of request with
           | "clone" -> session_reply request
           | "eval" -> asked := true; [status (id_of request) ["need-input"]]
           | "interrupt" ->
               [status (id_of request) ["done"];
                status "3" ["done"; "interrupted"]]
           | _ -> [status (id_of request) ["done"]])
      (fun port ->
         run ["-p"; string_of_int port]
           ~feed:(fun pid fd ->
               write "(read-line)\n" fd;
               let tries = ref 100 in
               while not !asked && !tries > 0 do
                 Thread.delay 0.05;
                 decr tries
               done;
               Thread.delay 1.0;
               Unix.kill pid Sys.sigint;
               Thread.delay 1.0))
  in
  assert (outcome = Unix.WEXITED 0);
  assert (List.map op_of requests
          = ["clone"; "describe"; "eval"; "interrupt"; "close"]);

  (* Something that isn't an nREPL server, or doesn't quite act like
     one, gets an error message rather than a crash. *)
  let fails_with script message =
    let (outcome, _, err), _ =
      with_tcp_server script
        (fun port -> run ["-p"; string_of_int port; "-e"; "1"])
    in
    outcome = Unix.WEXITED 1 && err = message ^ "\n"
  in
  assert (fails_with (fun _ -> [raw "HTTP/1.1 400 Bad Request\r\n\r\n"])
            "unexpected response from the server: \
             Bencode.read: unexpected character 'H'");
  assert (fails_with (fun request -> [status (id_of request) ["done"]])
            "unexpected response from the server: \
             nREPL server did not return a session");

  (* A host that doesn't resolve gets an error message too. *)
  let outcome, _, err =
    run ["--host"; "no-such-host.invalid"; "-p"; "1"; "-e"; "1"]
  in
  assert (outcome = Unix.WEXITED 1);
  assert (err
          = "cannot connect to no-such-host.invalid:1: \
             cannot resolve host \"no-such-host.invalid\"\n");

  print_endline "all cli tests passed"
