(* What a request has been sent while some other one was being read:
   a reply of its own, waiting for {!read_until_done}, or, while a
   request is read in the middle of another one's reading, anything
   that wasn't for the inner request, to be passed on in the order it
   came in once the outer reading resumes. *)
type queued = Reply of Bencode.t | Route of Bencode.t

(* A request still waiting for replies. [seq] orders requests by when
   they were sent. *)
type pending = {
  seq : int;
  on_reply : (Bencode.t -> unit) option;
  queue : queued Queue.t;
  mutable finished : bool;
}

(* The client does its own reading and writing on the descriptors
   (the channels are only kept for closing), so that a wait for the
   server can be cut short: to act on a Ctrl-C, or when time's up. *)
type t = {
  ic : in_channel;
  oc : out_channel;
  input : Unix.file_descr;
  output : Unix.file_descr;
  timeout : float option;
  buffer : Bytes.t;
  mutable start : int;
  mutable stop : int;
  other : Bencode.t -> unit;
  pending : (string, pending) Hashtbl.t;
  mutable reading : pending list;
  mutable session : string option;
  mutable counter : int;
}

let of_channels ?timeout ?(other = fun _ -> ()) ic oc =
  { ic; oc;
    input = Unix.descr_of_in_channel ic;
    output = Unix.descr_of_out_channel oc;
    timeout;
    buffer = Bytes.create 65536;
    start = 0;
    stop = 0;
    other;
    pending = Hashtbl.create 8;
    reading = [];
    session = None;
    counter = 0 }

let connect ?timeout ?other ~host ~port () =
  let addr =
    match
      Unix.getaddrinfo host (string_of_int port)
        [Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_FAMILY Unix.PF_INET]
    with
      | ai :: _ -> ai.Unix.ai_addr
      | [] -> Printf.ksprintf failwith "cannot resolve host %S" host
  in
  let sock = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  let close_on_error f =
    try f () with exn ->
      (try Unix.close sock with Unix.Unix_error _ -> ());
      raise exn
  in
  close_on_error (fun () ->
    match timeout with
      | Some seconds ->
          (* SO_SNDTIMEO doesn't reliably bound connect(2), notably
             not on macOS: use a non-blocking connect bounded by
             select instead, the way [wait] bounds the requests. *)
          Unix.set_nonblock sock;
          (try Unix.connect sock addr
           with Unix.Unix_error ((Unix.EINPROGRESS | Unix.EWOULDBLOCK), _, _) -> ());
          let _, writable, _ = Unix.select [] [sock] [] seconds in
          if writable = [] then
            raise (Unix.Unix_error (Unix.ETIMEDOUT, "connect", host));
          (match Unix.getsockopt_error sock with
             | Some error -> raise (Unix.Unix_error (error, "connect", host))
             | None -> ());
          Unix.clear_nonblock sock
      | None ->
          Unix.connect sock addr);
  of_channels ?timeout ?other
    (Unix.in_channel_of_descr sock) (Unix.out_channel_of_descr sock)

let close conn =
  (* Both channels share the socket fd: close one, discard the other. *)
  (try close_out conn.oc with Sys_error _ -> ());
  close_in_noerr conn.ic

let session conn = conn.session

(* Wait until the server's end can be read from (or written to), for no
   longer than the connection's timeout, calling [poll] first and
   every tenth of a second meanwhile. *)
let wait ?poll ~write conn =
  let until =
    Option.map (fun seconds -> Unix.gettimeofday () +. seconds) conn.timeout
  in
  let rec loop () =
    Option.iter (fun poll -> poll ()) poll;
    let left = Option.map (fun until -> until -. Unix.gettimeofday ()) until in
    (match left with
       | Some left when left <= 0.0 -> raise Sys_blocked_io
       | _ -> ());
    let timeout =
      match left, poll with
        | None, None -> -1.0
        | Some left, None -> left
        | None, Some _ -> 0.1
        | Some left, Some _ -> Float.min left 0.1
    in
    let reading, writing =
      if write then ([], [conn.output]) else ([conn.input], [])
    in
    match Unix.select reading writing [] timeout with
      | [], [], _ -> loop ()
      | _ -> ()
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let rec refill ?poll conn =
  wait ?poll ~write:false conn;
  match Unix.read conn.input conn.buffer 0 (Bytes.length conn.buffer) with
    | 0 -> raise End_of_file
    | n -> conn.start <- 0; conn.stop <- n
    | exception
        Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        refill ?poll conn

let read_message ?poll conn =
  let next () =
    if conn.start = conn.stop then refill ?poll conn;
    let c = Bytes.get conn.buffer conn.start in
    conn.start <- conn.start + 1;
    c
  in
  let take n =
    let text = Buffer.create n in
    while Buffer.length text < n do
      if conn.start = conn.stop then refill ?poll conn;
      let count = min (n - Buffer.length text) (conn.stop - conn.start) in
      Buffer.add_subbytes text conn.buffer conn.start count;
      conn.start <- conn.start + count
    done;
    Buffer.contents text
  in
  Bencode.read_from ~next ~take

let write conn text =
  let rec loop offset =
    if offset < String.length text then
      match
        wait ~write:true conn;
        Unix.single_write_substring conn.output text offset
          (String.length text - offset)
      with
        | written -> loop (offset + written)
        | exception
            Unix.Unix_error
              ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            loop offset
  in
  loop 0

let has_status msg status = List.mem status (Bencode.get_strings msg "status")

let is_done msg = has_status msg "done"

let send ?on_reply conn fields =
  conn.counter <- conn.counter + 1;
  let id = string_of_int conn.counter in
  let fields = ("id", Bencode.String id) :: fields in
  let fields =
    match conn.session with
      | Some session when not (List.mem_assoc "session" fields) ->
          ("session", Bencode.String session) :: fields
      | _ -> fields
  in
  let buf = Buffer.create 256 in
  Bencode.encode buf (Bencode.Dict fields);
  Hashtbl.replace conn.pending id
    { seq = conn.counter; on_reply; queue = Queue.create (); finished = false };
  write conn (Buffer.contents buf);
  id

(* Servers are meant to echo the id as it was sent, a string, but
   some send it back as an integer. *)
let response_id msg =
  match Bencode.get msg "id" with
    | Some (Bencode.String id) -> Some id
    | Some (Bencode.Int id) -> Some (string_of_int id)
    | _ -> None

(* The request a reply belongs to: the one it names, as long as that
   one still waits for replies. A server that leaves ids out can only
   be followed by order: its done ends the request sent last that's
   still waiting (the input for an evaluation is answered before the
   evaluation is done), except that an evaluation's interrupted done
   is the evaluation's, and anything else is the [current] read's. *)
let owner ?current conn msg =
  match response_id msg with
    | Some id -> if Hashtbl.mem conn.pending id then Some id else None
    | None when not (is_done msg) || has_status msg "interrupted" -> current
    | None ->
        Hashtbl.fold
          (fun id request last ->
             match last with
               | _ when request.finished -> last
               | Some (_, seq) when seq > request.seq -> last
               | _ -> Some (id, request.seq))
          conn.pending None
        |> Option.map fst

(* Hand a reply to the request it belongs to, other than the one being
   read, or to [other] when it belongs to none. *)
let rec deliver conn owner msg =
  match owner with
    | Some id when Hashtbl.mem conn.pending id ->
        let request = Hashtbl.find conn.pending id in
        if is_done msg then request.finished <- true;
        (match request.on_reply with
           | Some on_reply ->
               if is_done msg then Hashtbl.remove conn.pending id;
               on_reply msg
           | None -> Queue.push (Reply msg) request.queue)
    | _ -> conn.other msg

and forget conn id =
  match Hashtbl.find_opt conn.pending id with
    | Some request ->
        Hashtbl.remove conn.pending id;
        Queue.iter
          (function
            | Reply msg -> conn.other msg
            | Route msg -> deliver conn (owner conn msg) msg)
          request.queue
    | None -> ()

(* Responses are matched to their request by id rather than by
   order: an evaluation's output can trail its "done" (a future that
   prints after the eval returned), a reply can turn up after its
   request was given up on, and while an evaluation runs, the input it
   asked for and an interrupt for it are in flight too, as is a Tab
   completion at its stdin prompt. *)
let read_until_done ?poll conn ?(f = fun _ -> ()) id =
  let request =
    match Hashtbl.find_opt conn.pending id with
      | Some request -> request
      | None ->
          let request =
            { seq = conn.counter; on_reply = None; queue = Queue.create ();
              finished = false }
          in
          Hashtbl.replace conn.pending id request;
          request
  in
  (* Nothing reaches the outer reading, if any, before it resumes:
     that keeps replies in the order they came in, and nothing gets
     printed while the inner request's caller has the terminal (a Tab
     completion, with linenoise in raw mode). *)
  let outer = match conn.reading with outer :: _ -> Some outer | [] -> None in
  let rec next acc =
    match Queue.take_opt request.queue with
      | Some (Reply msg) -> handle acc msg
      | Some (Route msg) -> arrive acc msg
      | None -> arrive acc (read_message ?poll conn)
  and arrive acc msg =
    match owner ~current:id conn msg with
      | Some owner when owner = id -> handle acc msg
      | owner ->
          (match outer with
             | Some outer -> Queue.push (Route msg) outer.queue
             | None -> deliver conn owner msg);
          next acc
  and handle acc msg =
    f msg;
    if is_done msg then List.rev (msg :: acc) else next (msg :: acc)
  in
  (* However the reading ends, the request is off the books, and
     whatever it gets later (or got queued after its "done") is a
     stray. *)
  let finish () =
    conn.reading <- List.tl conn.reading;
    forget conn id
  in
  conn.reading <- request :: conn.reading;
  match next [] with
    | responses -> finish (); responses
    | exception e -> finish (); raise e

let request conn ?f fields = read_until_done conn ?f (send conn fields)

let clone conn =
  let responses = request conn [("op", Bencode.String "clone")] in
  match
    List.find_map (fun msg -> Bencode.get_string msg "new-session") responses
  with
    | Some session ->
        conn.session <- Some session;
        session
    | None ->
        failwith "nREPL server did not return a session"

let describe conn =
  request conn [("op", Bencode.String "describe")]

let send_eval conn code =
  send conn [("op", Bencode.String "eval"); ("code", Bencode.String code)]

let send_load_file conn ?file_name contents =
  send conn
    ([("op", Bencode.String "load-file"); ("file", Bencode.String contents)]
     @ (match file_name with
          | Some name -> [("file-name", Bencode.String name)]
          | None -> []))

let send_interrupt ?on_reply ?interrupt_id conn =
  send ?on_reply conn
    (("op", Bencode.String "interrupt")
     :: (match interrupt_id with
           | Some id -> [("interrupt-id", Bencode.String id)]
           | None -> []))

let send_stdin ?on_reply conn text =
  send ?on_reply conn
    [("op", Bencode.String "stdin"); ("stdin", Bencode.String text)]

let eval conn ?f code = read_until_done conn ?f (send_eval conn code)

let load_file conn ?f ?file_name contents =
  read_until_done conn ?f (send_load_file conn ?file_name contents)

let interrupt ?interrupt_id conn =
  read_until_done conn (send_interrupt ?interrupt_id conn)

let close_session conn =
  let responses = request conn [("op", Bencode.String "close")] in
  conn.session <- None;
  responses

let completions conn prefix =
  let responses =
    request conn
      [("op", Bencode.String "completions"); ("prefix", Bencode.String prefix)]
  in
  List.concat_map
    (fun msg ->
       match Bencode.get msg "completions" with
         | Some (Bencode.List candidates) ->
             List.filter_map
               (fun candidate -> Bencode.get_string candidate "candidate")
               candidates
         | _ -> [])
    responses

let eval_error responses =
  List.exists
    (fun msg -> List.mem "eval-error" (Bencode.get_strings msg "status"))
    responses

let versions responses =
  match List.find_map (fun msg -> Bencode.get msg "versions") responses with
    | Some (Bencode.Dict entries) ->
        List.map
          (fun (name, entry) ->
             (name,
              Option.value ~default:""
                (Bencode.get_string entry "version-string")))
          entries
    | _ -> []
