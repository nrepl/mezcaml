let () =
  (* Without a terminal on stdin there's nothing to change, and nothing
     stops a Ctrl-C typed elsewhere from being echoed there. *)
  let stdin = Unix.dup Unix.stdin in
  let r, w = Unix.pipe () in
  Unix.dup2 r Unix.stdin;
  let echoes = Terminal.keeping_typeahead Terminal.echoes in
  Unix.dup2 stdin Unix.stdin;
  List.iter Unix.close [stdin; r; w];
  assert echoes;

  print_endline "all terminal tests passed"
