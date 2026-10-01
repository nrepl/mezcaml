open Mezcaml
open Fake_server

(* Run an eval through [Evaluation] on a cloned session. [during] is
   called on every response, with the evaluation at hand (to press
   Ctrl-C, say). Returns the eval's responses and what got reported,
   along with the requests the server saw. *)
let evaluate ?(before = ignore) ?(during = fun _ _ -> ()) script =
  with_server
    (fun request ->
       if op_of request = "clone" then session_reply request
       else script request)
    (fun conn ->
       ignore (Client.clone conn : string);
       let reports = ref [] in
       let evaluation = Evaluation.eval conn "(loop [] (recur))" in
       before evaluation;
       let responses =
         Evaluation.run evaluation
           ~on_response:(fun msg -> during evaluation msg)
           ~report:(fun text -> reports := text :: !reports)
           ~read_input:(fun () -> None)
       in
       (evaluation, responses, List.rev !reports))

let interrupts requests =
  List.filter (fun request -> op_of request = "interrupt") requests

let () =
  (* The first Ctrl-C sends an interrupt naming the eval and its
     session, a second one means quit. A server that never answers the
     interrupt itself holds up neither this eval nor the next request,
     and a Ctrl-C after the eval is done is ignored. *)
  let presses = ref [] in
  let ((evaluation, responses, reports), next), requests =
    with_server
      (fun request ->
         match op_of request, id_of request with
           | "clone", _ -> session_reply request
           | "eval", "2" -> [reply "2" [("out", str "working\n")]]
           | "interrupt", _ -> [status "2" ["done"; "interrupted"]]
           | _ -> answer "3" request)
      (fun conn ->
         ignore (Client.clone conn : string);
         let reports = ref [] in
         let evaluation = Evaluation.eval conn "(loop [] (recur))" in
         let responses =
           Evaluation.run evaluation
             ~on_response:(fun _ ->
                 presses := Evaluation.ctrl_c evaluation :: !presses)
             ~report:(fun text -> reports := text :: !reports)
             ~read_input:(fun () -> None)
         in
         ((evaluation, responses, List.rev !reports),
          Client.eval conn "(+ 1 2)"))
  in
  assert (List.rev !presses = [`Interrupting; `Quit]);
  assert (responses
          = [reply "2" [("out", str "working\n")];
             status "2" ["done"; "interrupted"]]);
  assert (reports = ["evaluation interrupted"]);
  assert (next = [reply "4" [("value", str "3")]; status "4" ["done"]]);
  assert (interrupts requests
          = [message
               [("id", str "3"); ("interrupt-id", str "2");
                ("op", str "interrupt"); ("session", str "s1")]]);
  assert (Evaluation.ctrl_c evaluation = `Ignored);

  (* A Ctrl-C that comes before the eval has gone out interrupts it as
     soon as it has. *)
  let _, requests =
    evaluate
      ~before:(fun evaluation ->
          assert (Evaluation.ctrl_c evaluation = `Interrupting))
      (fun request ->
         if op_of request = "interrupt" then
           [status "3" ["done"]; status "2" ["done"; "interrupted"]]
         else [])
  in
  assert (List.map op_of requests = ["clone"; "eval"; "interrupt"]);

  (* A server that can't interrupt says so while the eval still runs,
     and the user hears about it. So do other failures. *)
  let can't_interrupt statuses =
    let (_, _, reports), _ =
      evaluate
        ~during:(fun evaluation _ ->
            ignore (Evaluation.ctrl_c evaluation
                    : [ `Interrupting | `Quit | `Ignored ]))
        (fun request ->
           match op_of request with
             | "eval" -> [reply "2" [("out", str "working\n")]]
             | _ ->
                 [status (id_of request) statuses;
                  reply "2" [("value", str "nil")]; status "2" ["done"]])
    in
    reports
  in
  assert (can't_interrupt ["error"; "unknown-op"; "done"]
          = ["the server can't interrupt evaluations; \
              Ctrl-C again quits mezcaml"]);
  assert (can't_interrupt ["interrupt-id-mismatch"; "done"]
          = ["the server couldn't interrupt the evaluation \
              (interrupt-id-mismatch)"]);
  assert (can't_interrupt ["error"; "done"]
          = ["the server couldn't interrupt the evaluation (error)"]);
  assert (can't_interrupt ["done"] = []);

  (* The SIGINT handler lasts only as long as the evaluation, and
     whatever was there before comes back. *)
  let mine _ = () in
  Sys.set_signal Sys.sigint (Sys.Signal_handle mine);
  let during, _ =
    with_server (answer "3")
      (fun conn ->
         let evaluation = Evaluation.eval conn "(+ 1 2)" in
         Evaluation.with_sigint evaluation (fun () ->
           let current = Sys.signal Sys.sigint Sys.Signal_default in
           Sys.set_signal Sys.sigint current;
           ignore (Evaluation.run evaluation ~on_response:ignore
                     ~report:ignore ~read_input:(fun () -> None)
                   : Bencode.t list);
           current))
  in
  assert (match during with
            | Sys.Signal_handle handler -> handler != mine
            | _ -> false);
  assert (match Sys.signal Sys.sigint Sys.Signal_default with
            | Sys.Signal_handle handler -> handler == mine
            | _ -> false);

  (* Real SIGINTs: the handler only takes note, and the evaluation acts
     on it. One that comes while the reading waits interrupts... *)
  let sigint_later seconds =
    Thread.create
      (fun () ->
         ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigint] : int list);
         Thread.delay seconds;
         Unix.kill (Unix.getpid ()) Sys.sigint)
      ()
  in
  let interrupted_by_signal ?delay ~code () =
    with_server ?delay
      (fun request ->
         match op_of request with
           | "eval" -> []
           | _ -> [status (id_of request) ["done"];
                   status "1" ["done"; "interrupted"]])
      (fun conn ->
         let evaluation = Evaluation.eval conn code in
         Evaluation.with_sigint evaluation (fun () ->
           let signal = sigint_later 0.2 in
           let responses =
             Evaluation.run evaluation ~on_response:ignore ~report:ignore
               ~read_input:(fun () -> None)
           in
           Thread.join signal;
           responses))
  in
  let responses, requests =
    interrupted_by_signal ~code:"(Thread/sleep 5000)" ()
  in
  assert (responses = [status "1" ["done"; "interrupted"]]);
  assert (List.map op_of requests = ["eval"; "interrupt"]);
  (* ...and so does one that comes while the eval itself is still
     going out, once it has... *)
  let big = String.make 4_000_000 ' ' in
  let responses, requests =
    interrupted_by_signal ~delay:0.5 ~code:("(Thread/sleep 5000)" ^ big) ()
  in
  assert (responses = [status "1" ["done"; "interrupted"]]);
  assert (List.map op_of requests = ["eval"; "interrupt"]);
  (* ...while one after the evaluation is done changes nothing. *)
  let _, requests =
    with_server (answer "3")
      (fun conn ->
         let evaluation = Evaluation.eval conn "(+ 1 2)" in
         Evaluation.with_sigint evaluation (fun () ->
           ignore (Evaluation.run evaluation ~on_response:ignore ~report:ignore
                     ~read_input:(fun () -> None)
                   : Bencode.t list);
           Unix.kill (Unix.getpid ()) Sys.sigint;
           try Thread.delay 0.1
           with Unix.Unix_error (Unix.EINTR, _, _) -> ()))
  in
  assert (List.map op_of requests = ["eval"]);

  (* Input the evaluation asks for goes to its session, without
     waiting for the stdin request's reply. *)
  let read_line ~answer script =
    with_server
      (fun request ->
         match op_of request with
           | "clone" -> session_reply request
           | "eval" -> [status "2" ["need-input"]]
           | _ -> script request)
      (fun conn ->
         ignore (Client.clone conn : string);
         Evaluation.run (Evaluation.eval conn "(read-line)")
           ~on_response:ignore ~report:ignore ~read_input:(fun () -> answer))
  in
  let responses, requests =
    read_line ~answer:(Some "hello\n")
      (fun request ->
         [status (id_of request) ["done"];
          reply "2" [("value", str "\"hello\"")]; status "2" ["done"]])
  in
  assert (responses
          = [status "2" ["need-input"];
             reply "2" [("value", str "\"hello\"")]; status "2" ["done"]]);
  assert (List.nth requests 2
          = message
              [("id", str "3"); ("op", str "stdin"); ("session", str "s1");
               ("stdin", str "hello\n")]);

  (* Ctrl-C at the prompt ([None]) sends nothing. *)
  let _, requests =
    with_server
      (fun request ->
         match op_of request with
           | "clone" -> session_reply request
           | _ ->
               [status "2" ["need-input"];
                status "2" ["done"; "interrupted"]])
      (fun conn ->
         ignore (Client.clone conn : string);
         Evaluation.run (Evaluation.eval conn "(read-line)")
           ~on_response:ignore ~report:ignore ~read_input:(fun () -> None))
  in
  assert (List.map op_of requests = ["clone"; "eval"]);

  (* Tab completion at the stdin prompt doesn't steal the evaluation's
     own replies. *)
  let (responses, candidates), _ =
    with_server
      (fun request ->
         match op_of request with
           | "eval" -> [status "1" ["need-input"]]
           | "completions" ->
               [reply "1" [("out", str "tick\n")];
                reply (id_of request)
                  [("completions",
                    Bencode.List [Bencode.Dict [("candidate", str "map")]]);
                   ("status", Bencode.List [str "done"])]]
           | _ ->
               [status (id_of request) ["done"];
                reply "1" [("value", str "\"m\"")]; status "1" ["done"]])
      (fun conn ->
         let candidates = ref [] in
         let responses =
           Evaluation.run (Evaluation.eval conn "(read-line)")
             ~on_response:ignore ~report:ignore
             ~read_input:(fun () ->
                 candidates := Client.completions conn "ma";
                 Some "m\n")
         in
         (responses, !candidates))
  in
  assert (candidates = ["map"]);
  assert (responses
          = [status "1" ["need-input"]; reply "1" [("out", str "tick\n")];
             reply "1" [("value", str "\"m\"")]; status "1" ["done"]]);

  (* UTF-8 characters are never cut in half. *)
  assert (Evaluation.utf8_boundary "" = 0);
  assert (Evaluation.utf8_boundary "abc" = 3);
  assert (Evaluation.utf8_boundary "a\xc3" = 1);
  assert (Evaluation.utf8_boundary "a\xc3\xa9" = 3);
  assert (Evaluation.utf8_boundary "\xe2\x82" = 0);
  assert (Evaluation.utf8_boundary "\xe2\x82\xac" = 3);
  assert (Evaluation.utf8_boundary "x\xf0\x9f\x98" = 1);
  assert (Evaluation.utf8_boundary "\xf0\x9f\x98\x80" = 4);
  (* Not UTF-8 at all: passed on as it is. *)
  assert (Evaluation.utf8_boundary "\x80\x80\x80\x80" = 4);

  (* Forwarded stdin keeps the tail of a split character for the next
     answer, reads on when a read brings nothing complete, and flushes
     what's left at the end of input before reporting it. *)
  let write fd text =
    ignore (Unix.write_substring fd text 0 (String.length text) : int)
  in
  let later fd text =
    Thread.create (fun () -> Thread.delay 0.1; write fd text) ()
  in
  let input, output = Unix.pipe () in
  let read = Evaluation.input_reader input in
  write output "a\xc3";
  assert (read () = Some "a");
  write output "\xa9b";
  assert (read () = Some "\xc3\xa9b");
  write output "\xe2";
  let writer = later output "\x82\xac" in
  assert (read () = Some "\xe2\x82\xac");
  Thread.join writer;
  write output "\xe2\x82";
  Unix.close output;
  assert (read () = Some "\xe2\x82");
  assert (read () = Some "");
  Unix.close input;

  (* A stdin left non-blocking gets waited on, not taken for its
     end. *)
  let input, output = Unix.pipe () in
  Unix.set_nonblock input;
  let read = Evaluation.input_reader input in
  let writer = later output "late\n" in
  assert (read () = Some "late\n");
  Thread.join writer;
  Unix.close output;
  Unix.close input;

  (* A signal (Ctrl-C, in real life) gets the evaluation out of a read
     that's still waiting for input. *)
  let input, output = Unix.pipe () in
  let read = Evaluation.input_reader input in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle ignore);
  ignore (Unix.setitimer Unix.ITIMER_REAL
            { Unix.it_interval = 0.0; it_value = 0.1 }
          : Unix.interval_timer_status);
  assert (read () = None);
  Sys.set_signal Sys.sigalrm Sys.Signal_default;
  Unix.close output;
  Unix.close input;

  (* A server that leaves ids out: an evaluation's input, a Tab
     completion at its stdin prompt and an interrupt for it all get
     through, by order. *)
  let bare fields = message fields in
  let bare_status statuses =
    bare [("status", Bencode.List (List.map str statuses))]
  in
  let (responses, candidates), requests =
    with_server
      (fun request ->
         match op_of request with
           | "eval" -> [bare_status ["need-input"]]
           | "completions" ->
               [bare [("completions",
                       Bencode.List [Bencode.Dict [("candidate", str "map")]])];
                bare_status ["done"]]
           | _ ->
               [bare_status ["done"]; bare [("value", str "\"m\"")];
                bare_status ["done"]])
      (fun conn ->
         let candidates = ref [] in
         let responses =
           Evaluation.run (Evaluation.eval conn "(read-line)")
             ~on_response:ignore ~report:ignore
             ~read_input:(fun () ->
                 candidates := Client.completions conn "ma";
                 Some "m\n")
         in
         (responses, !candidates))
  in
  assert (candidates = ["map"]);
  assert (responses
          = [bare_status ["need-input"]; bare [("value", str "\"m\"")];
             bare_status ["done"]]);
  assert (List.map op_of requests = ["eval"; "completions"; "stdin"]);
  let responses, _ =
    with_server
      (fun request ->
         match op_of request with
           | "eval" -> [bare [("out", str "working\n")]]
           | _ -> [bare_status ["done"]; bare_status ["done"; "interrupted"]])
      (fun conn ->
         let evaluation = Evaluation.eval conn "(loop [] (recur))" in
         Evaluation.run evaluation ~report:ignore ~read_input:(fun () -> None)
           ~on_response:(fun _ ->
               ignore (Evaluation.ctrl_c evaluation
                       : [ `Interrupting | `Quit | `Ignored ])))
  in
  assert (responses
          = [bare [("out", str "working\n")];
             bare_status ["done"; "interrupted"]]);

  print_endline "all evaluation tests passed"
