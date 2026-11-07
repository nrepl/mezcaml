(** The terminal on mezcaml's stdin, while the REPL has it. *)

val keeping_typeahead : (unit -> 'a) -> 'a
(** [keeping_typeahead f] runs [f], the REPL, with what's typed while
    linenoise isn't reading (during an evaluation, say) left for its
    next prompt as it is. In its usual mode the terminal would echo it
    over the evaluation's output, and turn Enter into a newline, which
    linenoise puts in the line rather than taking it for Enter. So
    until [f] returns, or mezcaml dies, the terminal neither echoes nor
    edits what's typed, and linenoise switches to raw mode and back
    from there. Ctrl-C still raises SIGINT, and SIGTERM, SIGHUP and
    SIGQUIT still end mezcaml (unless something else handles them),
    once the terminal has its settings back. Without a terminal on
    stdin, or with one linenoise doesn't edit lines on (TERM=dumb, say),
    it just runs [f]. *)

val die_of : int -> unit
(** Die of a signal the way mezcaml would without a handler for it,
    once the terminal has its settings back: dying of a signal skips
    [at_exit], which puts them back otherwise. *)

val echoes : unit -> bool
(** Whether the terminal on stdin echoes what's typed, which it doesn't
    while {!keeping_typeahead} runs. Without a terminal there, [true]:
    a Ctrl-C typed elsewhere gets echoed there. *)
