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

  print_endline "all repl_input tests passed"
