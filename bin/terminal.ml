(* The terminal's settings from before keeping_typeahead changed them,
   to put back however mezcaml ends. TCSANOW, as only input settings
   change, and putting them back mustn't wait on output a stuck
   terminal won't take. *)
let saved = ref None

let restore () =
  match !saved with
    | Some attrs ->
        saved := None;
        (try Unix.tcsetattr Unix.stdin Unix.TCSANOW attrs
         with Unix.Unix_error _ -> ())
    | None -> ()

let () = at_exit restore

let die_of signal =
  restore ();
  Sys.set_signal signal Sys.Signal_default;
  Unix.kill (Unix.getpid ()) signal

(* No echo, no line editing, and carriage returns left as they are, so
   that Enter is still Enter when linenoise reads it. Signals and
   output stay as they were. *)
let typeahead_mode (attrs : Unix.terminal_io) =
  { attrs with c_echo = false; c_icanon = false; c_icrnl = false }

(* linenoise's own list of terminals it doesn't edit lines on. It reads
   them with fgets, in the terminal's usual mode. *)
let dumb () =
  match Sys.getenv_opt "TERM" with
    | Some term ->
        List.mem (String.lowercase_ascii term) ["dumb"; "cons25"; "emacs"]
    | None -> false

(* The signals that would end mezcaml put the terminal back first. One
   that something else handles is left to it. *)
let restoring_on_fatal_signals f =
  let fatal =
    List.filter
      (fun signal ->
         match Sys.signal signal (Sys.Signal_handle die_of) with
           | Sys.Signal_default -> true
           | other -> Sys.set_signal signal other; false)
      [Sys.sigterm; Sys.sighup; Sys.sigquit]
  in
  Fun.protect f ~finally:(fun () ->
    List.iter (fun signal -> Sys.set_signal signal Sys.Signal_default) fatal)

let keeping_typeahead f =
  match Unix.tcgetattr Unix.stdin with
    | exception Unix.Unix_error _ -> f ()
    | _ when dumb () -> f ()
    | attrs ->
        (try
           Unix.tcsetattr Unix.stdin Unix.TCSANOW (typeahead_mode attrs);
           saved := Some attrs
         with Unix.Unix_error _ -> ());
        Fun.protect (fun () -> restoring_on_fatal_signals f) ~finally:restore

let echoes () =
  match Unix.tcgetattr Unix.stdin with
    | attrs -> attrs.c_echo
    | exception Unix.Unix_error _ -> true
