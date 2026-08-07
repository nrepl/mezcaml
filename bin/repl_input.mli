(** Input handling for the interactive REPL. *)

val split_for_completion : string -> string * string
(** [split_for_completion line] splits [line] into the part to keep
    verbatim and the trailing token to complete: ["(map str"] becomes
    [("(map ", "str")]. nREPL servers complete bare symbol prefixes
    while linenoise wants full-line replacements, so candidates are
    requested for the token and spliced back after the kept part. *)
