(** One eval or load-file request on behalf of the command line, from
    sending it to its "done": interrupting it when the user presses
    Ctrl-C and telling them what they need to know. *)

type t

val eval : Mezcaml.Client.t -> string -> t
(** An eval request for some code, to be sent by {!run}. *)

val load_file : Mezcaml.Client.t -> file_name:string -> string -> t
(** A load-file request for a file's contents, to be sent by {!run}. *)

val run :
  t -> on_response:(Mezcaml.Bencode.t -> unit) -> report:(string -> unit) ->
  read_input:(unit -> string option) -> Mezcaml.Bencode.t list
(** Send the request and read its responses until it's done, passing
    each one to [on_response] as it arrives, and return them. [report]
    gets what mezcaml itself has to say: an interrupted evaluation, or
    a server that couldn't interrupt it. [read_input] answers the
    server's requests for stdin (need-input): [Some text] to send,
    [Some ""] for end of input, or [None] to send nothing (the user
    pressed Ctrl-C instead). *)

val ctrl_c : t -> [ `Interrupting | `Quit | `Ignored ]
(** What a Ctrl-C does: the first one asks the server to interrupt the
    request (right after sending it, if it hasn't gone out yet), a
    second one means the user wants out, and one that comes once the
    request is done does nothing. *)

val press_ctrl_c : t -> unit
(** {!ctrl_c}, quitting the way an unhandled SIGINT would on [`Quit]:
    what a Ctrl-C does when it's read as a key press. *)

val ctrl_c_pending : t -> bool
(** Whether a SIGINT came in that hasn't been acted on yet. That tells
    a read it cut short (linenoise's, when it can't use the terminal)
    from the end of input. *)

val with_sigint : ?on_signal:(unit -> unit) -> t -> (unit -> 'a) -> 'a
(** [with_sigint t f] runs [f] with SIGINT standing for Ctrl-C, and
    restores the previous handler afterwards. The handler only notes
    the signal (and calls [on_signal], which mustn't do any more than
    that): {!run} acts on it as {!press_ctrl_c} would, from its
    reading. A second one quits right away. *)

val input_reader : Unix.file_descr -> unit -> string option
(** [input_reader fd] is a [read_input] for {!run} that passes on
    whatever arrives on [fd], mezcaml's own stdin. It never splits a
    UTF-8 character across two answers, waits on a descriptor left
    non-blocking, and yields [None] when a signal (Ctrl-C) interrupts
    the read. Read errors count as end of input. *)

val utf8_boundary : string -> int
(** The length of the longest prefix of a string that doesn't stop in
    the middle of a UTF-8 character. Bytes that aren't valid UTF-8 are
    left alone. *)
