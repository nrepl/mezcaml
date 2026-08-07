(** Input handling for the interactive REPL. *)

(** Which flavor of input the connected server evaluates, as far as
    reading whole forms is concerned. *)
type dialect =
  | Lisp  (** Clojure and friends: a form ends when brackets balance. *)
  | Erlang  (** Forms end with a [.] outside strings and comments. *)
  | Generic  (** Balanced brackets and closed strings, nothing more. *)

val dialect_of_versions : string list -> dialect
(** Pick the dialect from the names in [describe]'s version table
    (see [Mezcaml.Client.versions]). Unknown servers get [Generic],
    and so does Elixir on purpose: its do/end blocks can't be
    bracket-counted, and repartee also reports "erlang", where the
    dot rule must not win. *)

val complete : dialect -> string -> bool
(** Whether the input reads as a whole form, or the REPL should keep
    asking for continuation lines. A heuristic: when it sends too
    eagerly the server reports the syntax error it would have
    reported anyway, so it only has to be good, not perfect. *)

val split_for_completion : string -> string * string
(** [split_for_completion line] splits [line] into the part to keep
    verbatim and the trailing token to complete: ["(map str"] becomes
    [("(map ", "str")]. nREPL servers complete bare symbol prefixes
    while linenoise wants full-line replacements, so candidates are
    requested for the token and spliced back after the kept part. *)
