type t = {
  ic : in_channel;
  oc : out_channel;
  mutable session : string option;
  mutable counter : int;
}

let connect ~host ~port =
  let addr =
    match
      Unix.getaddrinfo host (string_of_int port)
        [Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_FAMILY Unix.PF_INET]
    with
      | ai :: _ -> ai.Unix.ai_addr
      | [] -> Printf.ksprintf failwith "cannot resolve host %S" host
  in
  let sock = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  (try Unix.connect sock addr
   with exn -> (try Unix.close sock with Unix.Unix_error _ -> ()); raise exn);
  { ic = Unix.in_channel_of_descr sock;
    oc = Unix.out_channel_of_descr sock;
    session = None;
    counter = 0 }

let close conn =
  (* Both channels share the socket fd: close one, discard the other. *)
  (try close_out conn.oc with Sys_error _ -> ());
  close_in_noerr conn.ic

let session conn = conn.session

let is_done msg = List.mem "done" (Bencode.get_strings msg "status")

let request conn ?(f = fun _ -> ()) fields =
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
  output_string conn.oc (Buffer.contents buf);
  flush conn.oc;
  let rec loop acc =
    let msg = Bencode.read conn.ic in
    f msg;
    if is_done msg then
      List.rev (msg :: acc)
    else
      loop (msg :: acc)
  in
  loop []

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

let eval conn ?f code =
  request conn ?f [("op", Bencode.String "eval"); ("code", Bencode.String code)]

let load_file conn ?f ?file_name contents =
  let fields =
    [("op", Bencode.String "load-file"); ("file", Bencode.String contents)]
    @ (match file_name with
         | Some name -> [("file-name", Bencode.String name)]
         | None -> [])
  in
  request conn ?f fields

let interrupt conn =
  request conn [("op", Bencode.String "interrupt")]

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
