open Mezcaml

let describe_response =
  Bencode.Dict
    [("versions",
      Bencode.Dict
        [("clojure", Bencode.Dict [("version-string", Bencode.String "1.12.5")]);
         ("nrepl", Bencode.Dict [("major", Bencode.Int 1)])])]

let () =
  assert (Client.versions [describe_response]
          = [("clojure", "1.12.5"); ("nrepl", "")]);

  (* The versions dict may sit in any response of the batch. *)
  assert (Client.versions
            [Bencode.Dict [("id", Bencode.String "1")]; describe_response]
          = [("clojure", "1.12.5"); ("nrepl", "")]);

  (* Missing or malformed versions yield an empty table. *)
  assert (Client.versions [] = []);
  assert (Client.versions [Bencode.Dict [("versions", Bencode.Int 3)]] = []);

  print_endline "all client tests passed"
