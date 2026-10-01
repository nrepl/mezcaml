open Mezcaml
open Fake_server

let describe_response =
  Bencode.Dict
    [("versions",
      Bencode.Dict
        [("clojure", Bencode.Dict [("version-string", Bencode.String "1.12.5")]);
         ("nrepl", Bencode.Dict [("major", Bencode.Int 1)])])]

let encoded msg =
  let buf = Buffer.create 64 in
  Bencode.encode buf msg;
  Buffer.contents buf

let () =
  assert (Client.versions [describe_response]
          = [("clojure", "1.12.5"); ("nrepl", "")]);

  (* The versions dict may sit in any response of the batch. *)
  assert (Client.versions
            [Bencode.Dict [("id", Bencode.String "1")]; describe_response]
          = [("clojure", "1.12.5"); ("nrepl", "")]);

  (* bb sends plain strings rather than version dicts. *)
  assert (Client.versions
            [Bencode.Dict
               [("versions",
                 Bencode.Dict
                   [("babashka", str "1.12.218");
                    ("babashka.nrepl", str "0.0.6")])]]
          = [("babashka", "1.12.218"); ("babashka.nrepl", "0.0.6")]);

  (* Missing or malformed versions yield an empty table. *)
  assert (Client.versions [] = []);
  assert (Client.versions [Bencode.Dict [("versions", Bencode.Int 3)]] = []);

  (* Failures: an eval-error status, an ex without one (nbb, let-go,
     shadow-cljs), or a request the server couldn't run. *)
  let ok = [reply "1" [("value", str "3")]; status "1" ["done"]] in
  assert (not (Client.eval_error ok));
  assert (Client.eval_error
            [reply "1" [("ex", str "Error: boom")]; status "1" ["done"]]);
  List.iter
    (fun failure -> assert (Client.eval_error [status "1" [failure; "done"]]))
    Client.failure_statuses;

  (* The server's err explains a failure when it comes with it or
     right next to it, in either order. *)
  let err text = reply "1" [("err", str text)] in
  let ex = reply "1" [("ex", str "class clojure.lang.ExceptionInfo");
                      ("status", Bencode.List [str "eval-error"])] in
  let done_1 = status "1" ["done"] in
  assert (Client.failure_message ok = None);
  assert (Client.failure_message [err "boom\n"; ex; done_1] = None);
  assert (Client.failure_message [ex; err "boom\n"; done_1] = None);
  assert (Client.failure_message
            [reply "1"
               [("err", str "boom"); ("ex", str "error:undef");
                ("status", Bencode.List [str "eval-error"; str "done"])]]
          = None);
  (* Otherwise it's the ex, even after some unrelated err output. *)
  let bare_ex = reply "1" [("ex", str "Error: boom")] in
  assert (Client.failure_message [bare_ex; done_1] = Some "Error: boom");
  assert (Client.failure_message
            [err "WARNING: reflection\n"; reply "1" [("out", str "x")];
             bare_ex; done_1]
          = Some "Error: boom");
  (* And failing that, a few words about the status. *)
  assert (Client.failure_message [status "1" ["eval-error"; "done"]]
          = Some "evaluation failed");
  assert (Client.failure_message
            [status "1" ["namespace-not-found"; "done"; "error"]]
          = Some "namespace not found");
  assert (Client.failure_message ~op:"load-file"
            [status "1" ["error"; "unknown-op"; "done"]]
          = Some "the server doesn't support load-file");
  assert (Client.failure_message [status "1" ["done"; "interrupted"]]
          = Some "evaluation interrupted");

  (* Ops come as a dict (nREPL) or as a plain list of names. *)
  assert (Client.ops
            [Bencode.Dict
               [("ops",
                 Bencode.Dict [("complete", Bencode.Dict []);
                               ("eval", Bencode.Dict [])])]]
          = ["complete"; "eval"]);
  assert (Client.ops
            [Bencode.Dict [("ops", Bencode.List [str "eval"; str "stdin"])]]
          = ["eval"; "stdin"]);
  assert (Client.ops [describe_response] = []);

  (* Completions come from whichever op the server has. *)
  assert (Client.completion_op ["completions"; "complete"; "eval"]
          = Some "completions");
  assert (Client.completion_op ["complete"; "eval"] = Some "complete");
  assert (Client.completion_op ["eval"] = None);
  assert (Client.completion_op [] = Some "completions");

  (* Completion requests carry the op and namespace asked for. *)
  let candidates, requests =
    with_server
      (fun request ->
         [reply (id_of request)
            [("completions",
              Bencode.List
                [Bencode.Dict [("candidate", str "map")];
                 Bencode.Dict [("candidate", str "mapv")]]);
             ("status", Bencode.List [str "done"])]])
      (fun conn -> Client.completions conn ~op:"complete" ~ns:"user" "ma")
  in
  assert (candidates = ["map"; "mapv"]);
  assert (requests
          = [message
               [("id", str "1"); ("ns", str "user"); ("op", str "complete");
                ("prefix", str "ma")]]);

  (* Requests carry an id and, once cloned, the session. *)
  let _, requests =
    with_server
      (fun request ->
         if op_of request = "clone" then
           [reply (id_of request)
              [("new-session", str "s1");
               ("status", Bencode.List [str "done"])]]
         else answer "3" request)
      (fun conn ->
         ignore (Client.clone conn : string);
         ignore (Client.eval conn "(+ 1 2)" : Bencode.t list))
  in
  assert (List.map
            (fun msg ->
               (Bencode.get_string msg "id", Bencode.get_string msg "session"))
            requests
          = [(Some "1", None); (Some "2", Some "s1")]);

  (* Output trailing an eval's "done" (a future printing after the
     eval returned) neither ends nor joins the next request: it's a
     stray. *)
  let late = reply "1" [("out", str "late\n")] in
  let strays = ref [] in
  let (first, second), _ =
    with_server ~other:(fun msg -> strays := msg :: !strays)
      (fun request ->
         if id_of request = "1" then answer "#future" request
         else late :: answer "3" request)
      (fun conn ->
         let first = Client.eval conn "(future (println \"late\"))" in
         (first, Client.eval conn "(+ 1 2)"))
  in
  assert (first
          = [reply "1" [("value", str "#future")]; status "1" ["done"]]);
  assert (second = [reply "2" [("value", str "3")]; status "2" ["done"]]);
  assert (!strays = [late]);

  (* Nor does a stray "done", say the reply to a completion request
     that timed out. *)
  let responses, _ =
    with_server (fun request -> status "7" ["done"] :: answer "3" request)
      (fun conn -> Client.eval conn "(+ 1 2)")
  in
  assert (responses = [reply "1" [("value", str "3")]; status "1" ["done"]]);

  (* Ids echoed as integers still match. *)
  let responses, _ =
    with_server
      (fun request ->
         let id = Bencode.Int (int_of_string (id_of request)) in
         [message [("id", id); ("value", str "3")];
          message [("id", id); ("status", Bencode.List [str "done"])]])
      (fun conn -> Client.eval conn "(+ 1 2)")
  in
  assert (List.length responses = 2);

  (* Replies with no id at all belong to the one request waiting for
     replies, rather than leave the client waiting forever... *)
  let done_ = message [("status", Bencode.List [str "done"])] in
  let responses, _ =
    with_server (fun _ -> [message [("value", str "3")]; done_])
      (fun conn -> Client.eval conn "(+ 1 2)")
  in
  assert (responses = [message [("value", str "3")]; done_]);

  (* A server that leaves ids out is followed by order: its "done"
     ends the request sent last, and the rest belongs to the one being
     read. So the input for an evaluation... *)
  let bare fields = message fields in
  let bare_status statuses =
    bare [("status", Bencode.List (List.map str statuses))]
  in
  let handled = ref [] in
  let responses, _ =
    with_server
      (fun request ->
         match op_of request with
           | "eval" -> [bare_status ["need-input"]]
           | _ ->
               [bare_status ["done"]; bare [("value", str "\"x\"")];
                bare_status ["done"]])
      (fun conn ->
         Client.eval conn "(read-line)"
           ~f:(fun msg ->
               if Client.has_status msg "need-input" then
                 ignore (Client.send conn
                           [("op", str "stdin"); ("stdin", str "x\n")]
                           ~on_reply:(fun msg -> handled := msg :: !handled)
                         : string)))
  in
  assert (responses
          = [bare_status ["need-input"]; bare [("value", str "\"x\"")];
             bare_status ["done"]]);
  assert (!handled = [bare_status ["done"]]);

  (* ...an interrupt for it, answered or not... *)
  let interrupted replies =
    let responses, _ =
      with_server
        (fun request ->
           if op_of request = "eval" then [bare [("out", str "working\n")]]
           else replies)
        (fun conn ->
           let sent = ref false in
           Client.eval conn "(loop [] (recur))"
             ~f:(fun _ ->
                 if not !sent then begin
                   sent := true;
                   ignore (Client.send conn [("op", str "interrupt")]
                             ~on_reply:ignore
                           : string)
                 end))
    in
    List.rev responses |> List.hd
  in
  assert (interrupted
            [bare_status ["done"]; bare_status ["done"; "interrupted"]]
          = bare_status ["done"; "interrupted"]);
  assert (interrupted [bare_status ["done"; "interrupted"]]
          = bare_status ["done"; "interrupted"]);

  (* ...and a Tab completion at its stdin prompt. *)
  let inner = ref [] in
  let outer, _ =
    with_server
      (fun request ->
         match op_of request with
           | "eval" -> [bare_status ["need-input"]]
           | "completions" ->
               [bare [("completions", Bencode.List [])]; bare_status ["done"]]
           | _ ->
               [bare_status ["done"]; bare [("value", str "\"m\"")];
                bare_status ["done"]])
      (fun conn ->
         Client.eval conn "(read-line)"
           ~f:(fun msg ->
               if Client.has_status msg "need-input" then begin
                 inner := Client.request conn [("op", str "completions")];
                 ignore (Client.send conn
                           [("op", str "stdin"); ("stdin", str "m\n")]
                           ~on_reply:ignore
                         : string)
               end))
  in
  assert (!inner
          = [bare [("completions", Bencode.List [])]; bare_status ["done"]]);
  assert (outer
          = [bare_status ["need-input"]; bare [("value", str "\"m\"")];
             bare_status ["done"]]);

  (* Replies to another request in flight go to its handler, and once
     it's done, whatever else turns up for it is a stray. *)
  let handled = ref [] and strays = ref [] in
  let responses, _ =
    with_server ~other:(fun msg -> strays := msg :: !strays)
      (fun request ->
         if op_of request = "eval" then
           [status "1" ["done"]; reply "1" [("out", str "late\n")];
            reply "2" [("value", str "3")]; status "2" ["done"]]
         else [])
      (fun conn ->
         ignore (Client.send conn [("op", str "stdin"); ("stdin", str "x")]
                   ~on_reply:(fun msg -> handled := msg :: !handled)
                 : string);
         Client.eval conn "(+ 1 2)")
  in
  assert (responses = [reply "2" [("value", str "3")]; status "2" ["done"]]);
  assert (!handled = [status "1" ["done"]]);
  assert (!strays = [reply "1" [("out", str "late\n")]]);

  (* A request made while another is being read (Tab completion at an
     evaluation's stdin prompt) gets its own replies, and the outer
     request's that come in meanwhile wait for it. *)
  let inner = ref [] in
  let outer, _ =
    with_server
      (fun request ->
         if op_of request = "eval" then [reply "1" [("out", str "1")]]
         else
           [reply "1" [("out", str "2")]; status "2" ["done"];
            reply "1" [("value", str "nil")]; status "1" ["done"]])
      (fun conn ->
         Client.eval conn "(do (print 1) (read-line))"
           ~f:(fun msg ->
               if Bencode.get_string msg "out" = Some "1" then
                 inner := Client.request conn [("op", str "completions")]))
  in
  assert (!inner = [status "2" ["done"]]);
  assert (outer
          = [reply "1" [("out", str "1")]; reply "1" [("out", str "2")];
             reply "1" [("value", str "nil")]; status "1" ["done"]]);

  (* Whatever reaches the outer request, or nobody, while the inner
     one is read comes out once the outer reading resumes, in the order
     it came in. *)
  let events = ref [] in
  let note what msg =
    let text = Option.value ~default:"-" (Bencode.get_string msg "out") in
    events := (what ^ " " ^ text) :: !events
  in
  let _ =
    with_server ~other:(note "stray")
      (fun request ->
         if op_of request = "eval" then [reply "1" [("out", str "start")]]
         else
           [reply "9" [("out", str "s1")]; reply "1" [("out", str "o1")];
            reply "9" [("out", str "s2")]; status "2" ["done"];
            status "1" ["done"]])
      (fun conn ->
         Client.eval conn "(read-line)"
           ~f:(fun msg ->
               note "eval" msg;
               if Bencode.get_string msg "out" = Some "start" then
                 ignore (Client.request conn [("op", str "completions")]
                         : Bencode.t list)))
  in
  assert (List.rev !events
          = ["eval start"; "stray s1"; "eval o1"; "stray s2"; "eval -"]);

  (* A request whose reading fails (a timeout, or here an exception
     from its own [f]) stops waiting: the rest of its reply, when it
     comes, is a stray. *)
  case "the late reply to a request whose reading failed" (fun () ->
    let strays = ref [] in
    let (failed, responses), _ =
      with_server ~other:(fun msg -> strays := msg :: !strays)
        (fun request ->
           if op_of request = "completions" then
             [reply (id_of request) [("out", str "thinking\n")]]
           else status "1" ["done"] :: answer "3" request)
        (fun conn ->
           let id = Client.send conn [("op", str "completions")] in
           let failed =
             match Client.read_until_done conn id ~f:(fun _ -> raise Exit) with
               | _ -> false
               | exception Exit -> true
           in
           (failed, Client.eval conn "(+ 1 2)"))
    in
    assert failed;
    assert (responses
            = [reply "2" [("value", str "3")]; status "2" ["done"]]);
    assert (!strays = [status "1" ["done"]]));

  (* A request given up on: what it got, and what it gets later, are
     strays. *)
  let strays = ref [] in
  let _ =
    with_server ~other:(fun msg -> strays := msg :: !strays)
      (fun request ->
         if id_of request = "2" then status "1" ["done"] :: answer "3" request
         else [])
      (fun conn ->
         let id = Client.send conn [("op", str "describe")] in
         Client.forget conn id;
         Client.eval conn "(+ 1 2)")
  in
  assert (!strays = [status "1" ["done"]]);

  (* Requests and replies far bigger than a socket buffer. *)
  let big = String.make 200_000 'x' in
  let responses, requests =
    with_server (answer big) (fun conn -> Client.eval conn big)
  in
  assert (List.map (fun msg -> Bencode.get_string msg "value") responses
          = [Some big; None]);
  assert (List.map (fun msg -> Bencode.get_string msg "code") requests
          = [Some big]);

  (* A server that never answers close can hold the client up only
     for as long as it's prepared to wait, even if it keeps sending
     other things meanwhile. Waiting for anything near the
     connection's own thirty seconds would mean it didn't give up. *)
  case "giving up on close" (fun () ->
    let gives_up script =
      let outcome, _ =
        with_server script
          (fun conn ->
             let started = Unix.gettimeofday () in
             match Client.close_session ~timeout:0.3 conn with
               | _ -> `Answered
               | exception Sys_blocked_io ->
                   `Gave_up (Unix.gettimeofday () -. started))
      in
      match outcome with
        | `Gave_up after -> after < 25.0
        | `Answered -> false
    in
    assert (gives_up (fun _ -> []));
    (* That's one deadline for the lot, also when a single reply
       trickles in. *)
    assert (gives_up (fun request ->
                let reply =
                  Bytes.of_string (encoded (status (id_of request) ["done"]))
                in
                List.concat
                  (List.init (Bytes.length reply) (fun i ->
                       [raw (String.make 1 (Bytes.get reply i)); pause 0.5]))));
    assert (gives_up (fun _ ->
                List.concat
                  (List.init 60 (fun _ ->
                       [pause 0.5; reply "9" [("out", str "tick\n")]])))));

  print_endline "all client tests passed"
