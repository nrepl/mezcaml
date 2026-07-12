(** A synchronous nREPL client connection.

    The client keeps one request in flight at a time: every operation
    sends a message and reads responses until the server signals
    "done". That is all a command-line client or a simple tool needs,
    and it keeps the implementation small. *)

type t

val connect : host:string -> port:int -> t
(** Open a TCP connection to an nREPL server. Raises
    [Unix.Unix_error] if the connection fails. *)

val close : t -> unit
(** Close the connection (the socket, not the nREPL session; for the
    latter see {!close_session}). *)

val session : t -> string option
(** The current nREPL session id, if {!clone} has been called. *)

val request :
  t -> ?f:(Bencode.t -> unit) -> (string * Bencode.t) list -> Bencode.t list
(** [request conn fields] sends one message (an "id" is added, and
    the current session if one exists) and reads responses until one
    carries a "done" status. Returns all responses, in order. [f], if
    given, is called on each response as it arrives, which is how a
    caller can stream output while a long evaluation runs. *)

(** {2 Protocol operations} *)

val clone : t -> string
(** Create a new session, remember it on the connection and return
    its id. Raises [Failure] if the server doesn't return one. *)

val describe : t -> Bencode.t list
val eval : t -> ?f:(Bencode.t -> unit) -> string -> Bencode.t list
val load_file : t -> ?f:(Bencode.t -> unit) -> ?file_name:string -> string -> Bencode.t list
val interrupt : t -> Bencode.t list
val close_session : t -> Bencode.t list

val completions : t -> string -> string list
(** Completion candidates for a prefix. Servers that don't implement
    the op yield [[]]. *)

(** {2 Response helpers} *)

val eval_error : Bencode.t list -> bool
(** Whether any response in the list carries an "eval-error" status. *)
