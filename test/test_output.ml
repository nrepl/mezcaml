open Fake_server

(* An Output writing to two temporary files, and a way to read back
   what each got. *)
let with_output ?(out_tty = false) ?(err_tty = false) f =
  let out_file = Filename.temp_file "mezcaml-out" ".txt" in
  let err_file = Filename.temp_file "mezcaml-err" ".txt" in
  let out = open_out_bin out_file and err = open_out_bin err_file in
  let output =
    Output.create ~out ~err ~out_tty ~err_tty ~out_color:false
      ~err_color:false ()
  in
  let contents file =
    flush out;
    flush err;
    let ic = open_in_bin file in
    let text = really_input_string ic (in_channel_length ic) in
    close_in ic;
    text
  in
  f output (fun () -> contents out_file) (fun () -> contents err_file);
  close_out out;
  close_out err;
  Sys.remove out_file;
  Sys.remove err_file

let () =
  (* Out as it is, err on stderr, values on a line of their own. *)
  with_output (fun output out err ->
    Output.print_response output
      (reply "1" [("out", str "hi"); ("err", str "oops\n")]);
    Output.print_response output (reply "1" [("value", str "\n\n3")]);
    assert (out () = "hi3\n");
    assert (err () = "oops\n"));

  (* Strays that turn up while linenoise owns the terminal wait for it
     to let go. *)
  with_output (fun output out _ ->
    let late = reply "1" [("out", str "late\n")] in
    Output.hold output (fun () -> Output.stray output late);
    assert (out () = "");
    Output.release output;
    assert (out () = "late\n");
    Output.release output;
    assert (out () = "late\n");
    Output.stray output late;
    assert (out () = "late\nlate\n"));

  (* mezcaml's messages start on a line of their own: after an
     unfinished line on the terminal, whichever stream left it... *)
  let err_text text = reply "1" [("err", str text)] in
  let out_text text = reply "1" [("out", str text)] in
  with_output ~out_tty:true ~err_tty:true (fun output out err ->
    Output.print_output output (err_text "boom");
    Output.report output "evaluation failed";
    assert (out () = "");
    assert (err () = "boom\nevaluation failed\n"));
  with_output ~out_tty:true ~err_tty:true (fun output out err ->
    Output.print_output output (out_text "name? ");
    Output.report output "evaluation interrupted";
    assert (out () = "name? \n");
    assert (err () = "evaluation interrupted\n"));
  (* ...after the ^C the terminal echoed, whatever comes next... *)
  with_output ~out_tty:true ~err_tty:true (fun output out err ->
    Output.ctrl_c_echoed output;
    Output.report output "evaluation interrupted";
    assert (out () = "");
    assert (err () = "\nevaluation interrupted\n"));
  with_output ~out_tty:true ~err_tty:true (fun output out _ ->
    Output.ctrl_c_echoed output;
    Output.print_output output (out_text "more\n");
    assert (out () = "\nmore\n"));
  (* ...and after an unfinished line in a redirected stderr. *)
  with_output (fun output _ err ->
    Output.print_output output (err_text "warning");
    Output.report output "evaluation failed";
    assert (err () = "warning\nevaluation failed\n"));
  (* A redirected stdout never gets a newline it didn't ask for. *)
  with_output ~err_tty:true (fun output out err ->
    Output.print_output output (out_text "x");
    Output.ctrl_c_echoed output;
    Output.report output "evaluation interrupted";
    assert (out () = "x");
    assert (err () = "\nevaluation interrupted\n"));

  (* mezcaml's own messages wait for linenoise too, in their place
     among the strays. *)
  with_output (fun output out err ->
    Output.hold output (fun () ->
      Output.stray output (out_text "late\n");
      Output.report output "the server can't interrupt evaluations");
    assert (out () = "" && err () = "");
    Output.release output;
    assert (out () = "late\n");
    assert (err () = "the server can't interrupt evaluations\n"));

  print_endline "all output tests passed"
