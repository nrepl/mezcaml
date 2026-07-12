# mezcaml

A minimal [nREPL](https://nrepl.org) client for OCaml. Distilled, a bit smoky,
and named after the only agave spirit with a camel in it.

nREPL is a message-based REPL protocol that originated in the Clojure world
and is spoken by a growing family of servers and editor clients
([CIDER](https://cider.mx), [neat](https://github.com/nrepl/neat),
[conjure](https://github.com/Olical/conjure) and friends). mezcaml is the
OCaml counterpart: a small client library plus a command-line tool, in the
spirit of neat's "do the protocol, skip the magic" philosophy. It pairs
nicely with utop's (experimental, for now) nREPL server mode, but it should
work against any nREPL server, whatever the language behind it.

## Status

Early days. The core protocol works, the API may still move around.

## Installation

Not on opam just yet. From a checkout:

```
dune build
dune install
```

## Command-line usage

Start an nREPL server somewhere. For OCaml that would be:

```
utop -nrepl 7888
```

Then:

```
$ mezcaml -p 7888
mezcaml, a minimal nREPL client. Ctrl-D to quit.
Connected to 127.0.0.1:7888 (nrepl 1.0, ocaml 5.3.0, utop 2.16.0)
mezcaml> 1 + 2
- : int = 3
mezcaml> let square x = x * x
val square : int -> int = <fun>
```

With no `-p`, mezcaml walks up from the current directory looking for the
`.nrepl-port` file that servers conventionally write, so a plain `mezcaml`
does the right thing from inside a project.

The REPL has line editing and persistent history (via linenoise; history
lives in `$XDG_STATE_HOME/mezcaml/history`), and Tab completion powered by
the server's `completions` op. Evaluation results are shown in green and
errors in red when stdout is a terminal; set `NO_COLOR` to turn that off.
Ctrl-C cancels the current line, Ctrl-D quits.

One-shot evaluation (exit code reflects success, handy for scripts):

```
$ mezcaml -e '21 * 2'
- : int = 42
```

Send a whole file:

```
$ mezcaml --load scratch.ml
```

For scripting there's also `--timeout SECONDS` (bounds connecting and each
wait for a response, so a wedged server can't hang your script forever),
`--color auto|always|never` (`always` is handy when piping into `less -R`),
and `--version`.

## Library usage

```ocaml
let () =
  let conn = Mezcaml.Client.connect ~host:"127.0.0.1" ~port:7888 () in
  let _session = Mezcaml.Client.clone conn in
  let responses = Mezcaml.Client.eval conn "1 + 2" in
  List.iter
    (fun msg ->
       match Mezcaml.Bencode.get_string msg "value" with
         | Some value -> print_string value
         | None -> ())
    responses;
  Mezcaml.Client.close conn
```

The library is three small modules:

- `Mezcaml.Bencode` - codec for the default nREPL transport
- `Mezcaml.Client` - synchronous connection and the protocol operations
  (`clone`, `describe`, `eval`, `load-file`, `completions`, `interrupt`,
  `close`)
- `Mezcaml.Port_file` - `.nrepl-port` discovery

## Limitations

By design, at least for now:

- Synchronous, one request in flight at a time. Fine for a CLI and simple
  tooling; an editor integration would want an async layer on top.
- No TLS, no EDN transport, only the default bencode one.
- Input is evaluated line by line; multi-line definitions have to go
  through `--load` for now.
- Ctrl-C cancels the line being edited but doesn't send the `interrupt`
  op during a running evaluation yet.

## License

MIT. See [LICENSE](LICENSE).
