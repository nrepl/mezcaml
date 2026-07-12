(** Discovery of running nREPL servers through .nrepl-port files,
    which servers conventionally write to their working directory. *)

val find : ?from:string -> unit -> int option
(** Walk from [from] (default: the current directory) up towards the
    filesystem root and return the port from the first [.nrepl-port]
    file found, if any. *)
