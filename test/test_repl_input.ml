open Repl_input

let () =
  (* Bare prefixes pass through untouched. *)
  assert (split_for_completion "ma" = ("", "ma"));
  assert (split_for_completion "clojure.st" = ("", "clojure.st"));
  assert (split_for_completion "Enum.ma" = ("", "Enum.ma"));

  (* The token starts after the last delimiter. *)
  assert (split_for_completion "(ma" = ("(", "ma"));
  assert (split_for_completion "(map str" = ("(map ", "str"));
  assert (split_for_completion "(-> x ma" = ("(-> x ", "ma"));
  assert (split_for_completion "X = lists:re" = ("X = ", "lists:re"));
  assert (split_for_completion "'ma" = ("'", "ma"));
  assert (split_for_completion "(str/jo" = ("(", "str/jo"));

  (* Prefix syntax that sits directly against a symbol delimits it. *)
  assert (split_for_completion "@ma" = ("@", "ma"));
  assert (split_for_completion "^Str" = ("^", "Str"));
  assert (split_for_completion "~ma" = ("~", "ma"));
  assert (split_for_completion "#'ma" = ("#'", "ma"));
  assert (split_for_completion "`ma" = ("`", "ma"));
  assert (split_for_completion "%En" = ("%", "En"));
  assert (split_for_completion "&Enum.ma" = ("&", "Enum.ma"));

  (* Nothing to complete. *)
  assert (split_for_completion "" = ("", ""));
  assert (split_for_completion "(foo)" = ("(foo)", ""));
  assert (split_for_completion "(map " = ("(map ", ""));

  (* Dialect detection from describe's version names. *)
  assert (dialect_of_versions ["clojure"; "java"; "nrepl"] = Lisp);
  assert (dialect_of_versions ["babashka"; "nrepl"] = Lisp);
  assert (dialect_of_versions ["jank"] = Lisp);
  assert (dialect_of_versions ["dialtone"; "erlang"; "nrepl"] = Erlang);
  (* repartee reports elixir alongside erlang; the dot rule must not win. *)
  assert (dialect_of_versions ["repartee"; "elixir"; "erlang"] = Generic);
  assert (dialect_of_versions ["nrepl"; "ocaml"; "utop"] = Generic);
  assert (dialect_of_versions [] = Generic);

  (* Lisp: a form ends when brackets balance outside strings. *)
  assert (complete Lisp "(+ 1 2)");
  assert (complete Lisp "42");
  assert (not (complete Lisp "(defn foo"));
  assert (complete Lisp "(defn square [x]\n  (* x x))");
  assert (not (complete Lisp "\"unclosed"));
  assert (not (complete Lisp "(str \"a)\""));
  assert (complete Lisp "(str \"a)\")");
  assert (not (complete Lisp "(foo ; )"));
  assert (complete Lisp "(+ 1 2) ; done)");
  assert (complete Lisp "(list \\( \\))");
  assert (not (complete Lisp ""));
  assert (not (complete Lisp "   "));
  assert (complete Lisp ")");

  (* Erlang: forms end with a dot outside strings and comments. *)
  assert (complete Erlang "1 + 2.");
  assert (not (complete Erlang "1 + 2"));
  assert (not (complete Erlang "Square = fun(X) ->"));
  assert (complete Erlang "Square = fun(X) ->\n  X * X\nend.");
  assert (not (complete Erlang "3.14"));
  assert (complete Erlang "3.14.");
  assert (complete Erlang "1 + 2. % done");
  assert (not (complete Erlang "io:format(\"a.b\")"));
  assert (complete Erlang "io:format(\"a.b\").");
  assert (not (complete Erlang "[1, 2."));
  assert (not (complete Erlang "'quoted.atom"));
  assert (complete Erlang "X = $..");
  assert (not (complete Erlang "X = $."));

  (* Generic: balanced brackets and closed strings. *)
  assert (complete Generic "1 + 2");
  assert (not (complete Generic "foo("));
  assert (not (complete Generic "\"open"));
  assert (complete Generic "[1, 2, 3]");

  print_endline "all repl_input tests passed"
