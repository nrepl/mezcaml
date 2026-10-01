(* A fake nREPL server for the tests, on the other end of a
   socketpair. *)

open Mezcaml

let str s = Bencode.String s

(* A message as the client reads it back: keys in wire (sorted)
   order. *)
let message fields =
  Bencode.Dict (List.sort (fun (a, _) (b, _) -> String.compare a b) fields)

let reply id fields = message (("id", str id) :: fields)

let status id statuses =
  reply id [("status", Bencode.List (List.map str statuses))]

let field name request =
  Option.value ~default:"" (Bencode.get_string request name)

let id_of = field "id"
let op_of = field "op"

(* The usual answer to an eval: one value, then done. *)
let answer value request =
  [reply (id_of request) [("value", str value)];
   status (id_of request) ["done"]]

(* A test that dies of an exception shows where. *)
let () = Printexc.record_backtrace true

(* [case name f] runs one test case, failing the run with its name when
   it raises rather than with a bare uncaught exception. *)
let case name f =
  try f () with e ->
    Printf.eprintf "%s: %s\n%s" name (Printexc.to_string e)
      (Printexc.get_backtrace ());
    exit 1

let session_reply request =
  [reply (id_of request)
     [("new-session", str "s1"); ("status", Bencode.List [str "done"])]]

(* Not a message: a script can return this among its replies to make
   the server pause for a while. *)
let pause seconds =
  Bencode.Dict [("fake-server", str "pause");
                ("seconds", str (string_of_float seconds))]

(* A client that goes away first mustn't kill the test run. *)
let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Answer each request read from [fd] with the replies [script]
   returns for it, until the client goes away. *)
let serve fd script requests =
  let ic = Unix.in_channel_of_descr fd in
  let oc = Unix.out_channel_of_descr fd in
  let send msg =
    match Bencode.get_string msg "fake-server" with
      | Some _ ->
          flush oc;
          Thread.delay (float_of_string (field "seconds" msg))
      | None ->
          let buf = Buffer.create 256 in
          Bencode.encode buf msg;
          output_string oc (Buffer.contents buf)
  in
  let rec loop () =
    match Bencode.read ic with
      | exception (End_of_file | Sys_error _) -> ()
      | request ->
          requests := request :: !requests;
          List.iter send (script request);
          flush oc;
          loop ()
  in
  (try loop () with Sys_error _ -> ());
  close_out_noerr oc

(* [with_server script run] runs [run] on a connection to a server
   that answers each request with the replies [script] returns for it.
   The server reads and writes on a thread of its own, so neither side
   blocks the other however much they send. Returns [run]'s result
   and the requests the server got, in order. The client's receive
   timeout (thirty seconds unless given, plenty for a slow machine)
   turns a script that never answers into a failure instead of a hung
   test. With [delay], the server waits that long before it starts
   reading. *)
let with_server ?(timeout = 30.0) ?(delay = 0.0) ?other script run =
  let client, server = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let requests = ref [] in
  let thread =
    Thread.create
      (fun () ->
         (* The signals the tests send themselves are for the client. *)
         ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigint; Sys.sigalrm]
                 : int list);
         Thread.delay delay;
         serve server script requests)
      ()
  in
  let conn =
    Client.of_channels ~timeout ?other (Unix.in_channel_of_descr client)
      (Unix.out_channel_of_descr client)
  in
  let result =
    Fun.protect
      ~finally:(fun () -> Client.close conn; Thread.join thread)
      (fun () -> run conn)
  in
  (result, List.rev !requests)
