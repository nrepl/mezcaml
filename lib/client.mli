(** A synchronous nREPL client connection.

    Every operation sends a message and reads responses until the
    server signals "done" for it. Replies are matched to requests by
    id, so a few requests can be in flight at once (an evaluation, the
    input it asked for, an interrupt for it), and output an earlier
    evaluation printed after it finished doesn't end a later request.
    Reading stays synchronous, which is all a command-line client or a
    simple tool needs, and it keeps the implementation small. *)

type t

val connect :
  ?timeout:float -> ?other:(Bencode.t -> unit) -> host:string -> port:int ->
  unit -> t
(** Open a TCP connection to an nREPL server. Raises
    [Unix.Unix_error] if the connection fails.

    [other] gets the replies that belong to no request still waiting
    for them: output an evaluation printed after its "done", say, or
    the reply to a request that was given up on. A client would print
    their output. By default they are dropped.

    [timeout], in seconds, bounds both connecting and each subsequent
    wait for the server, to read a response or to take a request.
    Without it, a server that accepts the connection but never answers
    blocks forever. When a wait times out the pending {!request}
    raises [Sys_blocked_io]. *)

val of_channels :
  ?timeout:float -> ?other:(Bencode.t -> unit) -> in_channel ->
  out_channel -> t
(** Wrap an already open pair of channels to a server, for transports
    other than TCP (or a fake server in tests). [timeout] and [other]
    are as for {!connect}. The client reads and writes their
    descriptors itself, so the channels shouldn't be used for anything
    but this. *)

val close : t -> unit
(** Close the connection (the socket, not the nREPL session; for the
    latter see {!close_session}). *)

val session : t -> string option
(** The current nREPL session id, if {!clone} has been called. *)

val request :
  t -> ?f:(Bencode.t -> unit) -> (string * Bencode.t) list -> Bencode.t list
(** [request conn fields] is {!send} followed by {!read_until_done}. *)

val send :
  ?on_reply:(Bencode.t -> unit) -> t -> (string * Bencode.t) list -> string
(** [send conn fields] sends one message without waiting for a reply
    and returns its id. An "id" is added, and the current session if
    one exists and [fields] has none.

    The request then waits for its replies until its "done". Replies
    that arrive while another request is being read are handed to
    [on_reply] if it's given: that's for requests nobody reads the
    replies of, like the input for an evaluation. Without it, they're
    kept for {!read_until_done}, so the caller has to read the request
    sooner or later, or {!forget} it. *)

val read_until_done :
  ?poll:(unit -> unit) -> t -> ?f:(Bencode.t -> unit) -> string ->
  Bencode.t list
(** [read_until_done conn id] reads the replies to request [id] until
    one carries a "done" status, and returns them in order. [f], if
    given, is called on each as it arrives, which is how a caller can
    stream output while a long evaluation runs. Replies to the other
    requests in flight that come in meanwhile go where {!send} says,
    and the rest to the connection's [other] handler (see {!connect}).
    When this is a request read in the middle of another one's
    reading (from [f]), they're passed on in order once the outer
    reading resumes. However the reading ends, the request stops
    waiting: anything it gets later goes to [other].

    A server that leaves ids out of its replies can only be followed
    by order. A "done" without an id ends the request sent last that
    still waits for one (unless it says "interrupted", which only an
    evaluation does), and other replies without an id belong to the
    request being read.

    [poll], if given, is called before each wait for the server, and
    every tenth of a second while it lasts: a chance to act on what a
    signal handler noted, since the handler itself can't safely send
    anything. *)

val forget : t -> string -> unit
(** Stop waiting for the replies to request [id]: the ones it got and
    any still to come go to the connection's [other] handler. *)

(** {2 Protocol operations} *)

val clone : t -> string
(** Create a new session, remember it on the connection and return
    its id. Raises [Failure] if the server doesn't return one. *)

val describe : t -> Bencode.t list
val eval : t -> ?f:(Bencode.t -> unit) -> string -> Bencode.t list
val load_file : t -> ?f:(Bencode.t -> unit) -> ?file_name:string -> string -> Bencode.t list

val interrupt : ?interrupt_id:string -> t -> Bencode.t list
(** Interrupt the session's evaluation [interrupt_id] (a request id),
    or whatever it is evaluating when there's none. *)

val close_session : t -> Bencode.t list

val completions : t -> string -> string list
(** Completion candidates for a prefix. Servers that don't implement
    the op yield [[]]. *)

(** {2 Sending without waiting}

    For callers that read the replies themselves (see {!send} and
    {!read_until_done}). While an evaluation runs, an interrupt for it
    has to go out without waiting for a reply. Each returns the id of
    the message sent. *)

val send_eval : t -> string -> string
val send_load_file : t -> ?file_name:string -> string -> string

val send_interrupt :
  ?on_reply:(Bencode.t -> unit) -> ?interrupt_id:string -> t -> string

(** {2 Response helpers} *)

val has_status : Bencode.t -> string -> bool
(** Whether a response carries the given status. *)

val eval_error : Bencode.t list -> bool
(** Whether any response in the list carries an "eval-error" status. *)

val versions : Bencode.t list -> (string * string) list
(** The version table from {!describe} responses, as (name,
    version-string) pairs: [("clojure", "1.12.5"); ("nrepl", "1.7.0")].
    A name whose entry carries no version-string maps to [""]. Empty
    when the responses hold no versions dictionary. *)
