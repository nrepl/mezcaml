open Mezcaml

type t = {
  conn : Client.t;
  send : unit -> string;
  mutable id : string option;
  mutable finished : bool;
  mutable interrupt : [ `No | `Requested | `Sent of string ];
  (* A Ctrl-C the SIGINT handler noted, for the reading to act on. *)
  mutable signalled : bool;
  mutable report : string -> unit;
}

let create conn send =
  { conn; send; id = None; finished = false; interrupt = `No;
    signalled = false; report = ignore }

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

(* Once the evaluation is done, its interrupt's reply is a stray like
   any other: a server that answers late, or never, mustn't hold up
   the next request. *)
let run t ~on_response ~report =
  t.report <- report;
  let id = t.send () in
  t.id <- Some id;
  if t.interrupt = `Requested then send_interrupt t id;
  let f msg =
    on_response msg;
    if Client.has_status msg "interrupted" then report "evaluation interrupted"
  in
  Fun.protect
    (fun () ->
       Client.read_until_done t.conn ~f id ~poll:(fun () -> act_on_signal t))
    ~finally:(fun () ->
      t.finished <- true;
      match t.interrupt with
        | `Sent id -> Client.forget t.conn id
        | `No | `Requested -> ())

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
