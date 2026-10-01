open Mezcaml

type t = {
  conn : Client.t;
  send : unit -> string;
  mutable id : string option;
  mutable finished : bool;
  mutable interrupt : [ `No | `Requested | `Sent of string ];
  (* A Ctrl-C the SIGINT handler noted, for the reading to act on. *)
  mutable signalled : bool;
  (* The input sent for the evaluation that hasn't been answered. *)
  mutable inputs : string list;
  mutable report : string -> unit;
}

let create conn send =
  { conn; send; id = None; finished = false; interrupt = `No;
    signalled = false; inputs = []; report = ignore }

let eval conn code = create conn (fun () -> Client.send_eval conn code)

let load_file conn ~file_name contents =
  create conn (fun () -> Client.send_load_file conn ~file_name contents)

let interrupt_reply t msg =
  let failures =
    List.filter (fun status -> status <> "done" && status <> "error")
      (Bencode.get_strings msg "status")
  in
  if Client.has_status msg "unknown-op" then
    t.report
      "the server can't interrupt evaluations; Ctrl-C again quits mezcaml"
  else if Client.has_status msg "session-idle" then
    t.report "nothing to interrupt, the evaluation had already finished"
  else if failures <> [] || Client.has_status msg "error" then
    t.report
      (Printf.sprintf "the server couldn't interrupt the evaluation (%s)"
         (String.concat ", " (if failures = [] then ["error"] else failures)))

(* [`Requested] while it's being written, so that a Ctrl-C that comes
   meanwhile counts as the second one. *)
let send_interrupt t id =
  t.interrupt <- `Requested;
  t.interrupt <-
    `Sent
      (Client.send_interrupt t.conn ~interrupt_id:id
         ~on_reply:(interrupt_reply t))

let ctrl_c t =
  if t.finished then `Ignored
  else
    match t.interrupt, t.id with
      | (`Requested | `Sent _), _ -> `Quit
      | `No, None -> t.interrupt <- `Requested; `Interrupting
      | `No, Some id -> send_interrupt t id; `Interrupting

(* Die the way an unhandled SIGINT would. *)
let quit () =
  Sys.set_signal Sys.sigint Sys.Signal_default;
  Unix.kill (Unix.getpid ()) Sys.sigint

let press_ctrl_c t =
  match ctrl_c t with
    | `Quit -> quit ()
    | `Interrupting | `Ignored -> ()

(* Run from the reading, where sending is safe. *)
let act_on_signal t =
  if t.signalled then begin
    t.signalled <- false;
    press_ctrl_c t
  end

let ctrl_c_pending t =
  (* An allocation lets a signal that came in during a C call
     (linenoise's) run its handler first. *)
  ignore (Sys.opaque_identity (ref ()) : unit ref);
  t.signalled

let send_input t text =
  let id = ref "" in
  let on_reply msg =
    if Client.has_status msg "done" then
      t.inputs <- List.filter (fun input -> input <> !id) t.inputs
  in
  id := Client.send_stdin t.conn text ~on_reply;
  t.inputs <- !id :: t.inputs

(* Once the evaluation is done, the replies to its interrupt and its
   input are strays like any other: a server that answers late, or
   never, mustn't hold up the next request. *)
let run t ~on_response ~report ~read_input =
  t.report <- report;
  let id = t.send () in
  t.id <- Some id;
  if t.interrupt = `Requested then send_interrupt t id;
  let f msg =
    on_response msg;
    if Client.has_status msg "interrupted" then report "evaluation interrupted";
    if Client.has_status msg "need-input" then
      match read_input () with
        | Some text -> send_input t text
        | None -> ()
  in
  Fun.protect
    (fun () ->
       Client.read_until_done t.conn ~f id ~poll:(fun () -> act_on_signal t))
    ~finally:(fun () ->
      t.finished <- true;
      (match t.interrupt with
         | `Sent id -> Client.forget t.conn id
         | `No | `Requested -> ());
      List.iter (Client.forget t.conn) t.inputs)

(* The handler itself only takes note, as it may run in the middle of
   anything, a request being sent included. Quitting is the exception:
   it touches nothing, and it has to work however stuck mezcaml is. *)
let on_sigint t =
  if t.finished then ()
  else if t.signalled || t.interrupt <> `No then quit ()
  else t.signalled <- true

let with_sigint ?(on_signal = ignore) t f =
  let previous =
    Sys.signal Sys.sigint
      (Sys.Signal_handle (fun _ -> on_signal (); on_sigint t))
  in
  Fun.protect f ~finally:(fun () -> Sys.set_signal Sys.sigint previous)

let utf8_boundary s =
  let n = String.length s in
  (* The last character starts at most three continuation bytes back. *)
  let rec start i =
    if i < 0 || n - i > 4 then None
    else if Char.code s.[i] land 0xC0 = 0x80 then start (i - 1)
    else Some i
  in
  match start (n - 1) with
    | None -> n
    | Some i ->
        let c = Char.code s.[i] in
        let length =
          if c land 0xE0 = 0xC0 then 2
          else if c land 0xF0 = 0xE0 then 3
          else if c land 0xF8 = 0xF0 then 4
          else 1
        in
        if i + length > n then i else n

(* Unix.read rather than the stdin channel: unlike the channel, it
   gives up on EINTR, so a Ctrl-C gets the evaluation out of a read
   that would otherwise go on waiting for input. *)
let input_reader fd =
  let buf = Bytes.create 4096 in
  let pending = ref "" in
  (* End of input: first whatever is left of a split character, then
     "". *)
  let finish () =
    let rest = !pending in
    pending := "";
    Some rest
  in
  let rec read () =
    match Unix.read fd buf 0 (Bytes.length buf) with
      | 0 -> finish ()
      | n ->
          let data = !pending ^ Bytes.sub_string buf 0 n in
          let complete = utf8_boundary data in
          pending := String.sub data complete (String.length data - complete);
          if complete = 0 then read () else Some (String.sub data 0 complete)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> None
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          (* A parent left stdin non-blocking: wait for it like a
             blocking read would. *)
          (match Unix.select [fd] [] [] (-1.0) with
             | _ -> read ()
             | exception Unix.Unix_error (Unix.EINTR, _, _) -> None)
      | exception Unix.Unix_error _ -> finish ()
  in
  read
