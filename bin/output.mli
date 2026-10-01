(** Printing what the server sends the way a terminal user expects:
    evaluation output as it was written, errors in red on stderr,
    values in green on their own line, and mezcaml's own messages on a
    line of their own.

    It keeps track of where each stream left off, and of where the
    terminal's cursor is when they go to one, so that a message can
    start on a fresh line without adding a newline to a redirected
    stream. *)

type t

val create :
  ?out:out_channel -> ?err:out_channel -> ?out_tty:bool -> ?err_tty:bool ->
  out_color:bool -> err_color:bool -> unit -> t
(** Output to [out] and [err] (stdout and stderr by default), colored
    where asked for. Whether each one goes to a terminal is found out
    unless given. *)

val print_response : t -> Mezcaml.Bencode.t -> unit
(** A response to the request being read: its out, err and value. *)

val print_output : t -> Mezcaml.Bencode.t -> unit
(** Just the out and err of a response. *)

val report : t -> string -> unit
(** One of mezcaml's own messages, on a line of its own on stderr. *)

val fresh_line : t -> unit
(** Make sure the terminal's cursor is at the start of a line, before
    a prompt: linenoise draws its prompt from column 0, wiping
    whatever was left on that line. *)

val end_line : t -> unit
(** A newline on stdout. *)

val ctrl_c_echoed : t -> unit
(** Note that the terminal echoed a ^C: what's printed next starts on
    a new line. *)

val stray : t -> Mezcaml.Bencode.t -> unit
(** A reply that belongs to no request still waiting for it (a future
    printing after its eval finished): its output gets printed, unless
    it's being held (see {!hold}). *)

val hold : t -> (unit -> 'a) -> 'a
(** [hold t f] runs [f], holding back strays and reports meanwhile.
    That's for requests made while linenoise has the terminal in raw
    mode (Tab completion), where printing would garble the line being
    edited. *)

val release : t -> unit
(** Print what was held back so far, in the order it came in. *)
