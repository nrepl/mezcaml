open Fake_server

(* An Output writing to two temporary files, and a way to read back
   what each got. *)
let with_output f =
  let out_file = Filename.temp_file "mezcaml-out" ".txt" in
  let err_file = Filename.temp_file "mezcaml-err" ".txt" in
  let out = open_out_bin out_file and err = open_out_bin err_file in
  let output = Output.create ~out ~err ~out_color:false ~err_color:false () in
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

  print_endline "all output tests passed"
