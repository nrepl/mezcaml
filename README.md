# mezcaml

A minimal [nREPL](https://nrepl.org) client for OCaml. Distilled, a bit smoky,
and named after the only agave spirit with a camel in it.

nREPL is a message-based REPL protocol that originated in the Clojure world
and is spoken by a growing family of servers and editor clients
([CIDER](https://cider.mx), [neat](https://github.com/nrepl/neat),
[conjure](https://github.com/Olical/conjure) and friends). mezcaml is the
OCaml counterpart: a small client library plus a command-line tool, in the
spirit of neat's "do the protocol, skip the magic" philosophy. It works
against any nREPL server, whatever the language on the other end - the
Clojure reference implementation, the BEAM servers
([dialtone](https://github.com/nrepl/nrepl-beam) for Erlang, repartee for
Elixir), and anything else that speaks the protocol.

## Status

Early days. The core protocol works, the API may still move around.

## Installation

Not on opam just yet. From a checkout:

```
dune build
dune install
```

## Command-line usage

Start an nREPL server somewhere. The Clojure reference server, for
instance:

```
clj -Sdeps '{:deps {nrepl/nrepl {:mvn/version "1.7.0"}}}' -M -m nrepl.cmdline --port 7888
```

Then:

```
$ mezcaml -p 7888
mezcaml 0.1.0, a minimal nREPL client. Ctrl-D to quit.
Connected to 127.0.0.1:7888 (clojure 1.12.5, java 21.0.5, nrepl 1.7.0)
mezcaml> (+ 1 2)
3
mezcaml> (defn square [x]
    ...>   (* x x))
#'user/square
mezcaml> (square 7)
49
```

The language on the other end makes no difference. Here's the same
client talking to [dialtone](https://github.com/nrepl/nrepl-beam), the
Erlang server (started with `bin/dialtone --port 7888`):

```
$ mezcaml -p 7888
mezcaml 0.1.0, a minimal nREPL client. Ctrl-D to quit.
Connected to 127.0.0.1:7888 (dialtone 0.1.0, erlang 29, nrepl 1.0.0)
mezcaml> Square = fun(X) -> X * X end.
#Fun<erl_eval.42.130099583>
mezcaml> Square(7).
49
```

With no `-p`, mezcaml walks up from the current directory looking for the
`.nrepl-port` file that servers conventionally write, so a plain `mezcaml`
does the right thing from inside a project.

The REPL has line editing and persistent history (via linenoise; history
lives in `$XDG_STATE_HOME/mezcaml/history`), and Tab completion powered by
the server's `completions` op. It reads whole forms, not lines: the
`...>` prompt keeps going until brackets balance (or, on an Erlang
server, until the closing `.`). Evaluation results are shown in green and
errors in red when stdout is a terminal; set `NO_COLOR` to turn that off.

### Key bindings

The usual emacs-flavored readline bindings work:

| Keys | Action |
|---|---|
| Ctrl-A / Ctrl-E | start / end of line |
| Ctrl-B / Ctrl-F | move by character |
| Alt-B / Alt-F, Ctrl-Left / Ctrl-Right | move by word |
| Ctrl-W, Alt-Backspace | delete the word before the cursor |
| Alt-D | delete the word after the cursor |
| Ctrl-K | delete to the end of the line |
| Ctrl-U | delete the whole line |
| Ctrl-T | transpose characters |
| Up / Down, Ctrl-P / Ctrl-N | walk the history |
| Ctrl-R | incremental reverse history search; Ctrl-R again for older matches, Enter keeps the match, Ctrl-G puts your line back |
| Ctrl-L | clear the screen |
| Tab | completion, powered by the server |
| Ctrl-C | cancel the input being edited, or interrupt a running evaluation |
| Ctrl-D | delete the character under the cursor; on an empty line, quit |

No kill ring and no undo - that's where linenoise draws the line, and
mezcaml with it.

Ctrl-C during an evaluation sends the server an `interrupt` for it
(with `-e` and `--load` too). Not every server can interrupt one
(Babashka and Basilisp can't), and there the evaluation simply runs
to its end. A second Ctrl-C quits mezcaml.

One-shot evaluation (exit code reflects success, handy for scripts):

```
$ mezcaml -e '(* 21 2)'
42
```

Send a whole file:

```
$ mezcaml --load scratch.clj
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
  let responses = Mezcaml.Client.eval conn "(+ 1 2)" in
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
- A server that leaves ids out of its replies gets followed by the
  order they come in, which can't always be right. If it never
  answers an interrupt, the evaluation can end up waiting for a
  "done" it already got, until `--timeout` or a second Ctrl-C.
- Whole-form reading is a heuristic: balanced brackets and strings for
  Lisp-family servers, the terminating `.` for Erlang. Elixir's
  `do ... end` blocks aren't recognized, so multi-line Elixir goes
  through `--load`.

## License

MIT. See [LICENSE](LICENSE).
